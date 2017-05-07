module Swine.Surface.Parser where

import           Swine.Prelude
import           Swine.Parser
import           Swine.Surface.Exp
import           Swine.Prim

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
            swineReserve "->"
            e <- parseExp
            return (LamType None head e)
        , parseApps head
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
    , parens parseExp
    , Hole <$ swineReserve "?"
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

parseRecordType :: (SwineParsing m) => m Exp
parseRecordType = (RecordType <$> (symbolic '{' >> recordFields)) <?> "record type"
  where
    recordFields = asum
      [ symbolic '}' >> return mempty
      , do
          lbl <- swineIdent
          symbolic ':'
          e <- parseExp
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
parseNamedArg = (parens (do
  binders <- try $ do
    binders <- (:) <$> parseBinder <*> many parseBinder
    symbolic ':'
    return binders
  ty <- parseExp
  return [(Some binder, ty) | binder <- binders])) <?> "function type argument"

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
