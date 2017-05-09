module Swine.Surface.Parser where

import           Swine.Prelude
import           Swine.Parser
import           Swine.Surface.Exp
import           Swine.Prim

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
    , Hole None <$ swineReserve "?"
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
parseRecord = (Record <$> (symbolic '{' >> recordFields)) <?> "record"
  where
    recordFields = asum
      [ symbolic '}' >> return mempty
      , do
          lbl <- swineIdent
          symbolic '='
          e <- parseExp
          symbolic ';'
          map (Pair lbl e :<) recordFields
      ]

parseRecordType :: (SwineParsing m) => m Type
parseRecordType = (parseRecordType_ RecordType parseExp) <?> "record type"

parseRecordType_ :: (SwineParsing m) => (Fwd (Pair Label a) -> b) -> m a -> m b
parseRecordType_ f p = f <$> (symbolic '{' >> recordFields)
  where
    recordFields = asum
      [ symbolic '}' >> return mempty
      , do
          lbl <- swineIdent
          symbolic ':'
          e <- p
          symbolic ';'
          map (Pair lbl e :<) recordFields
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
parseVariantType = (VariantType <$> (symbolic '[' >> variantFields)) <?> "variant type"
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
                map (Pair lbl (Some e) :<) variantFields
            , map (Pair lbl None :<) variantFields
            ]
      ]

parseLam :: (SwineParsing m) => m Exp
parseLam = (do
  symbolic '\\'
  go) <?> "lambda"
  where
    go = do
      Pair pat mbType <- parseTypedPattern
      body <- asum
        [ swineReserve "->" >> parseExp
        , go
        ]
      return (Lam pat mbType body)

parseTypedPattern :: (SwineParsing m) => m (Pair Pattern (Option Type))
parseTypedPattern = (asum
  [ do
      pat <- parsePattern
      return (Pair pat None)
  , parens $ do
      pat <- parsePattern
      symbolic ':'
      ty <- parseExp
      return (Pair pat (Some ty))
  ]) <?> "typed pattern"

parsePattern :: (SwineParsing m) => m Pattern
parsePattern = (asum
  [ PatBinder <$> parseBinder
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
  , do
      symbolic '{'
      let go = asum
            [ symbolic '}' >> return FwdNil
            , do
                lbl <- swineIdent
                asum
                  [ do
                      symbolic ';'
                      (Pair lbl None :<) <$> go
                  , do
                      symbolic '='
                      pat <- parsePattern
                      symbolic ';'
                      (Pair lbl (Some pat) :<) <$> go
                  ]
            ]
      PatRecord <$> go
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
  pars <- listToFwd <$> many parseTypedPattern
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
      [ PropEmpty <$ swineReserve "Empty"
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
