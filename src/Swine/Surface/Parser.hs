module Swine.Surface.Parser where

import           Swine.Prelude
import           Swine.Parser
import           Swine.Surface.Exp
import           Swine.Prim
import           Swine.Binder
import qualified Swine.LookupList as LL

parseFunctionArgument :: (SwineParsing m) => m Exp
parseFunctionArgument = do
  head <- parseExpArg
  args <- many parseExpArg
  return (foldl' App head args)

parseExp :: (SwineParsing m) => m Exp
parseExp = (asum
  [ parseLam
  , parseLamTypeStartingWithNamedArg
  , parseCase
  , parseLet
  , Prim <$> swinePrim
  , parsePrimOp
  , parseCompound
  ]) <?> "expression"
  where
    parseCompound = do
      e1 <- parseFunctionArgument
      asum
        [ do
            swineReserve "->"
            e2 <- parseCompound
            return (LamType None e1 e2)
        , return e1
        ]

parseExpArg :: (SwineParsing m) => m Exp
parseExpArg = do
  e <- asum
    [ Var <$> parseVar
    , Type <$ swineReserve "Type"
    , parseRecord
    , parseRecordType
    , parseVariant
    , parseVariantType
    , PrimType <$> swinePrimType
    , Hole <$ swineReserve "?"
    , parsePropType
    , parens parseExp
    , parseCoe
    , parseAxiom
    ]
  asum
    [ parseProjs e
    , return e
    ]
  where
    parseProjs e = asum
      [ do
          symbolic '.'
          lbl <- swineIdent
          parseProjs (Proj e lbl)
      , return e
      ]

parseVar :: (SwineParsing m) => m Var
parseVar = swineIdent

parseRecord :: (SwineParsing m) => m Exp
parseRecord = (do
  flds <- parseLookupList =<< (symbolic '{' >> recordFields)
  return (Record flds)) <?> "record"
  where
    recordFields = asum
      [ symbolic '}' >> return mempty
      , do
          lbl <- swineIdent
          symbolic '='
          e <- parseExp
          symbolic ';'
          map ((lbl, e) :) recordFields
      ]

parseRecordType :: (SwineParsing m) => m Type
parseRecordType = (parseRecordType_ RecordType parseExp) <?> "record type"

parseRecordType_ :: (SwineParsing m) => (LookupList Label a -> b) -> m a -> m b
parseRecordType_ f p = do
  flds <- parseLookupList =<< (symbolic '{' >> recordFields)
  return (f flds)
  where
    recordFields = asum
      [ symbolic '}' >> return mempty
      , do
          lbl <- swineIdent
          symbolic ':'
          e <- p
          symbolic ';'
          map ((lbl, e) :) recordFields
      ]

parseVariant :: (SwineParsing m) => m Exp
parseVariant = (do
  symbolic '['
  lbl <- swineIdent
  asum
    [ do
        symbolic ']'
        return (Variant lbl None)
    , do
        symbolic '='
        e <- parseExp
        symbolic ']'
        return (Variant lbl (Some e))
    ]) <?> "variant"

parseVariantType :: (SwineParsing m) => m Exp
parseVariantType = (do
  flds <- parseLookupList =<< (symbolic '[' >> variantFields)
  return (VariantType flds)) <?> "variant type"
  where
    variantFields = asum
      [ symbolic ']' >> return mempty
      , do
          lbl <- swineIdent
          asum
            [ do
                symbolic ':'
                e <- parseExp
                symbolic ';'
                map ((lbl, Some e) :) variantFields
            , map ((lbl, None) :) variantFields
            ]
      ]

parseLam :: (SwineParsing m) => m Exp
parseLam = (do
  symbolic '\\'
  go) <?> "lambda"
  where
    go = do
      pat <- fix parseIrrPattern
      body <- asum
        [ swineReserve "->" >> parseExp
        , go
        ]
      return (Lam pat body)

parseIrrPattern :: (SwineParsing m) => m (Pattern irr) -> m (Pattern irr)
parseIrrPattern parsePat = (asum
  [ PatBinder <$> parseBinder
  , do
      symbolic '{'
      let go = asum
            [ symbolic '}' >> return []
            , do
                lbl <- swineIdent
                asum
                  [ do
                      symbolic ';'
                      ((lbl, RFPPun None) :) <$> go
                  , do
                      symbolic ':'
                      ty <- parseExp
                      symbolic ';'
                      ((lbl, RFPPun (Some ty)) :) <$> go
                  , do
                      symbolic '='
                      pat <- parsePat
                      symbolic ';'
                      ((lbl, RFPNormal pat) :) <$> go
                  ]
            ]
      PatRecord <$> (parseLookupList =<< go)
  , do
      symbolic '('
      pat <- parsePat
      symbolic ':'
      ty <- parseExp
      symbolic ')'
      return (PatTyped pat ty)
  ]) <?> "irrefutable pattern"

-- TODO prim pattern
parsePattern :: (SwineParsing m) => m (Pattern 'False)
parsePattern = (asum
  [ parseIrrPattern parsePattern
  , do
      symbolic '['
      lbl <- swineIdent
      asum
        [ symbolic ']' >> return (PatVariant lbl None)
        , do
            symbolic '='
            pat <- parsePattern
            symbolic ']'
            return (PatVariant lbl (Some pat))
        ]
  ]) <?> "pattern"

parseBinder :: (SwineParsing m) => m Binder
parseBinder = (asum
  [ Bind <$> swineIdent
  , Ignore <$> swineIgnore
  ]) <?> "binder"

parseLamTypeStartingWithNamedArg :: (SwineParsing m) => m Type
parseLamTypeStartingWithNamedArg = do
  args <- concat <$> ((:) <$> parseNamedArg <*> many parseNamedArg)
  let go = \case
        [] -> do
          swineReserve "->"
          parseExp
        (mbBind, argTy) : rest -> LamType mbBind argTy <$> go rest
  go args

parseNamedArg :: (SwineParsing m) => m [(Option Binder, Type)]
parseNamedArg = (try (parens (do
  binders <- do
    binders <- (:) <$> parseBinder <*> many parseBinder
    symbolic ':'
    return binders
  ty <- parseExp
  return [(Some binder, ty) | binder <- binders]))) <?> "function type named argument"

parseCase :: (SwineParsing m) => m Exp
parseCase = (do
  swineReserve "case"
  e <- parseExpArg -- We need this otherwise the { gets interpreted as start of record
  symbolic '{'
  alts <- parseAlts
  return (Case e alts)) <?> "case"
  where
    parseAlts = asum
      [ symbolic '}' >> return FwdNil
      , do
          pat <- parsePattern
          swineReserve "->"
          body <- parseExp
          symbolic ';'
          (Pair pat body :<) <$> parseAlts
      ]

parseLet :: (SwineParsing m) => m Exp
parseLet = (do
  swineReserve "let"
  n <- parseBinder
  pars <- listToFwd <$> many (fix parseIrrPattern)
  retTy <- asum
    [ do
        symbolic ':'
        ty <- parseExp
        symbolic '='
        return (Some ty)
    , do
        symbolic '='
        return None
    ]
  body <- parseExp
  symbolic ';'
  e <- parseExp
  return (Let n pars retTy body e)) <?> "let"

parsePrimOp :: (SwineParsing m) => m Exp
parsePrimOp = (do
  pop <- swinePrimOp
  args <- listToFwd <$> replicateM (primOpArity pop) parseExpArg
  return (PrimOp pop args)) <?> "prim op"

parsePropType :: (SwineParsing m) => m Exp
parsePropType = do
  symbolic '|'
  p <- parseProp
  symbolic '|'
  return (PropType p)

parseProp :: (SwineParsing m) => m Prop
parseProp = (asum
  [ parseSimple
  , parseForallStartingWithNamedArg
  , parseForall
  ]) <?> "prop"
  where
    parseSimple = asum
      [ do
          symbolic '['
          symbolic ']'
          return PropEmpty
      , parsePropProduct
      , do
          swineReserve "TyEq"
          ty1 <- parseExpArg
          ty2 <- parseExpArg
          return (PropTypeEq ty1 ty2)
      , do
          swineReserve "ValEq"
          e1 <- parseExpArg
          ty1 <- parseExpArg
          e2 <- parseExpArg
          ty2 <- parseExpArg
          return (PropValEq ty1 e1 ty2 e2)
      ]

    parseForallStartingWithNamedArg = do
      args <- concat <$> ((:) <$> parseNamedArg <*> many parseNamedArg)
      let go = \case
            [] -> do
              swineReserve "->"
              parseProp
            (mbBind, argTy) : rest -> PropForall mbBind argTy <$> go rest
      go args

    parseForall = do
      argTy <- parseFunctionArgument
      swineReserve "->"
      p <- parseProp
      return (PropForall None argTy p)

parsePropProduct :: (SwineParsing m) => m Prop
parsePropProduct = (parseRecordType_ PropProduct parseProp) <?> "prop product"

parsePropArg :: (SwineParsing m) => m Prop
parsePropArg = asum
  [ PropEmpty <$ swineReserve "Empty"
  , parens parseProp
  ]

parseCoe :: (SwineParsing m) => m Exp
parseCoe = do
  swineReserve "coe"
  ty1 <- parseExpArg
  ty2 <- parseExpArg
  eq <- parseExpArg
  val <- parseExpArg
  return (Coe ty1 ty2 eq val)

parseAxiom :: (SwineParsing m) => m Exp
parseAxiom = do
  swineReserve "axiom"
  p <- parseProp
  return (Axiom p)

parseLookupList :: (SwineParsing m) => [(Label, a)] -> m (LookupList Label a)
parseLookupList xs = case LL.fromList xs of
  Left x -> fail ("Duplicate label " <> show x)
  Right xs' -> return xs'
