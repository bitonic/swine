module Swine.Surface.Parser where

import           Swine.Prelude
import           Swine.Parser
import           Swine.Surface.Exp
import           Swine.Prim

parseExp :: (SwineParsing m) => m Exp
parseExp = asum
  [ parseLam
  , parseLamType
  , parseCase
  , parseLet
  , Prim <$> swinePrim
  , parsePrimOp
  , parseCompound
  ]
  where
    parseProjs e = asum
      [ do
          symbolic '.'
          lbl <- swineIdent
          parseProjs (Proj e lbl)
      , return e
      ]

    parseApps e = asum
      [ do
          arg <- parseExpArg
          parseApps (App e arg)
      , return e
      ]

    parseCompound = do
      head <- parseExpArg
      asum
        [ do
            symbolic ':'
            ty <- parseExp
            return (Annotated head ty)
        , parseProjs head
        , parseApps head
        ]

parseExpArg :: (SwineParsing m) => m Exp
parseExpArg = asum
  [ Var <$> parseVar
  , Type <$ swineReserve "Type"
  , parseRecord
  , parseRecordType
  , parseVariant
  , parseVariantType
  , PrimType <$> swinePrimType
  , parens parseExp
  , Hole <$ swineReserve "?"
  ]

parseVar :: (SwineParsing m) => m Var
parseVar = swineIdent

parseRecord :: (SwineParsing m) => m Exp
parseRecord = Record <$> (symbolic '{' >> recordFields)
  where
    recordFields = asum
      [ symbolic '}' >> return mempty
      , do
          lbl <- swineIdent
          symbolic '='
          e <- parseExp
          symbolic ','
          map (Pair lbl e :<) recordFields
      ]

parseRecordType :: (SwineParsing m) => m Exp
parseRecordType = RecordType <$> (symbolic '{' >> recordFields)
  where
    recordFields = asum
      [ symbolic '}' >> return mempty
      , do
          lbl <- swineIdent
          symbolic ':'
          e <- parseExp
          symbolic ','
          map (Pair lbl e :<) recordFields
      ]

parseVariant :: (SwineParsing m) => m Exp
parseVariant = do
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
    ]

parseVariantType :: (SwineParsing m) => m Exp
parseVariantType = VariantType <$> (symbolic '[' >> variantFields)
  where
    variantFields = asum
      [ symbolic ']' >> return mempty
      , do
          lbl <- swineIdent
          asum
            [ do
                symbolic ':'
                e <- parseExpArg
                map (Pair lbl (Some e) :<) variantFields
            , map (Pair lbl None :<) variantFields
            ]
      ]

parseLam :: (SwineParsing m) => m Exp
parseLam = do
  symbolic '\\'
  go
  where
    go = do
      Pair pat mbType <- parseTypedPattern
      body <- asum
        [ swineReserve "->" >> parseExp
        , go
        ]
      return (Lam pat mbType body)

parseTypedPattern :: (SwineParsing m) => m (Pair Pattern (Option Type))
parseTypedPattern = asum
  [ do
      pat <- parsePattern
      return (Pair pat None)
  , parens $ do
      pat <- parsePattern
      symbolic ':'
      ty <- parseExp
      return (Pair pat (Some ty))
  ]

parsePattern :: (SwineParsing m) => m Pattern
parsePattern = asum
  [ PatBinder <$> parseBinder
  , do
      symbolic '['
      lbl <- swineIdent
      asum
        [ symbolic ']' >> return (PatVariant lbl None)
        , do
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
                      symbolic ','
                      (Pair lbl None :<) <$> go
                  , do
                      symbolic '='
                      pat <- parsePattern
                      symbolic ','
                      (Pair lbl (Some pat) :<) <$> go
                  ]
            ]
      PatRecord <$> go
  ]

parseBinder :: (SwineParsing m) => m Binder
parseBinder = asum
  [ Bind <$> swineIdent
  , Ignore <$> swineIgnore
  ]

parseLamType :: (SwineParsing m) => m Type
parseLamType = do
  args <- asum
    [ concat <$> many parseNamedArg
    , do
        ty <- parseExpArg
        return [(None, ty)]
    ]
  swineReserve "->"
  res <- parseExp
  let go = \case
        [] -> res
        (mbBind, argTy) : rest -> LamType mbBind argTy (go rest)
  return (go args)
  where
    parseNamedArg :: (SwineParsing m) => m [(Option Binder, Type)]
    parseNamedArg = (do
      binders <- try $ do
        binders <- (:) <$> parseBinder <*> many parseBinder
        symbolic ':'
        return binders
      ty <- parseExp
      return [(Some binder, ty) | binder <- binders]) <?> "function type argument"

parseCase :: (SwineParsing m) => m Exp
parseCase = do
  swineReserve "case"
  e <- parseExpArg -- We need this otherwise the { gets interpreted as start of record
  symbolic '{'
  alts <- parseAlts
  return (Case e alts)
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
parseLet = do
  swineReserve "let"
  n <- parseBinder
  pars <- listToFwd <$> many (parens parseTypedPattern)
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
  symbolic '='
  body <- parseExp
  symbolic ';'
  e <- parseExp
  return (Let n pars retTy body e)

parsePrimOp :: (SwineParsing m) => m Exp
parsePrimOp = do
  pop <- swinePrimOp
  args <- listToFwd <$> replicateM (primOpArity pop) parseExpArg
  return (PrimOp pop args)
