{-# LANGUAGE FlexibleContexts #-}
module Swine.Parser where

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import qualified Data.CharSet.Unicode as Unicode
import qualified Data.HashSet as HS
import           Control.Monad.Except
import qualified Data.HashMap.Strict as HMS

import           Swine.Prelude
import qualified Swine.Exp as E

data ParseError
  = PEOutOfScope
      Text -- What we looked for

type SwineParsing m = (TokenParsing m, MonadError ParseError m)
type ParseMonad m = Either

data Env a where
  ENil :: HashMap Text a -> Env a
  ECons :: Text -> Env a -> Env (E.Var a)

envLookup :: Env a -> Text -> Maybe a
envLookup env0 txt = case env0 of
  ENil hm -> HMS.lookup txt hm
  ECons txt' env -> if txt == txt'
    then return (E.B (E.Binder txt))
    else E.F <$> envLookup env txt

parseExp :: (SwineParsing m) => Env a -> m (E.Exp a)
parseExp env = (asum
  [ parseLam env
  , do
      head <- parseArg env
      args <- many (parseArg env)
      return (foldl' E.App head args)
  ]) <?> "expression"

parseArg :: (SwineParsing m) => Env a -> m (E.Exp a)
parseArg env = (asum
  [ E.var <$> parseVar env
  , parens (parseExp env)
  ]) <?> "argument"

parseVar :: (SwineParsing m) => Env a -> m a
parseVar env = do
  txt <- swineIdent
  case envLookup env txt of
    Nothing -> throwError (PEOutOfScope txt)
    Just v -> return v

parseLam :: (SwineParsing m) => Env a -> m (E.Exp a)
parseLam env = do
  void (symbolic '\\')
  v <- swineIdent
  void (symbol "->")
  body <- parseExp (ECons v env)
  return (E.lam (E.Binder v) body)

swineIdent :: (SwineParsing m) => m Text
swineIdent = ident swineIdentStyle

swineIdentStyle :: (SwineParsing m) => IdentifierStyle m
swineIdentStyle = IdentifierStyle
  { _styleName = "swine"
  , _styleStart = oneOfSet swineIdentCharsStart
  , _styleLetter = oneOfSet swineIdentCharsLetter
  , _styleReserved = HS.fromList
      [ "case", "->"
      ]
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

swineIdentCharsStart :: CharSet
swineIdentCharsStart = CharSet.difference
  (Unicode.letter <> Unicode.mark <> Unicode.symbol <> Unicode.punctuation)
  -- Remove things that we need for parsing
  (CharSet.fromList ['\'', '\\', '"', '{', '}', '[', ']', '(', ')', ',', ';'])

-- We exclude numbers from the beginning since otherwise we could have
-- variables that look like number literals
swineIdentCharsLetter :: CharSet
swineIdentCharsLetter = swineIdentCharsStart <> Unicode.number

