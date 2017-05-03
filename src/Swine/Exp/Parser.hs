{-# LANGUAGE FlexibleContexts #-}
module Swine.Exp.Parser
  ( module Swine.Exp.Parser
  , eof
  ) where

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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Trifecta as Trifecta
import qualified Text.Trifecta.Delta as Trifecta

import           Swine.Prelude
import qualified Swine.Exp as E
import qualified Swine.Pretty as P

data ParseError
  = PEOutOfScope
      Text -- What we looked for

class (TokenParsing m, Monad m) => SwineParsing m where
  throwParseError :: ParseError -> m a

newtype SwineParseMonad a = SwineParseMonad
  { unSwineParseMonad :: Trifecta.Parser a }
  deriving (Functor, Applicative, Monad, TokenParsing, CharParsing, Parsing, Alternative)

instance P.Pretty ParseError where
  pretty = \case
    PEOutOfScope txt -> "Out of scope identifier" P.<+> P.text txt

instance SwineParsing SwineParseMonad where
  throwParseError pe =
    SwineParseMonad (Trifecta.raiseErr (Trifecta.Err (Just (P.pretty pe)) [] mempty mempty))

runSwineParsing ::
     FilePath
  -> Int -- ^ Line
  -> Int -- ^ Column
  -> ByteString -- ^ Input
  -> (forall m. (SwineParsing m) => m a)
  -> Either P.Doc a
runSwineParsing fp line col s (SwineParseMonad p) = let
  delta = Trifecta.Directed (T.encodeUtf8 (T.pack fp)) (fromIntegral line - 1) (fromIntegral col - 1) 0 0
  in case Trifecta.parseByteString p delta s of
    Trifecta.Success a -> Right a
    Trifecta.Failure err -> Left (Trifecta._errDoc err)

data Env a where
  ENil :: HashMap Text a -> Env a
  ECons :: Maybe Text -> Env a -> Env (E.Var a)

envLookup :: Env a -> Text -> Maybe a
envLookup env0 txt = case env0 of
  ENil hm -> HMS.lookup txt hm
  ECons txt' env -> if Just txt == txt'
    then return (E.B (E.Binder txt))
    else E.F <$> envLookup env txt

parseExp :: (SwineParsing m) => Env a -> m (E.Exp a)
parseExp env = (asum
  [ parseLam env
  , parseLet env
  , E.prim <$> parsePrim
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
    Nothing -> throwParseError (PEOutOfScope txt)
    Just v -> return v

patName :: E.Pattern -> Maybe Text
patName = \case
  E.PatBind (E.Binder txt) -> Just txt
  E.PatIgnore{} -> Nothing

parseLam :: (SwineParsing m) => Env a -> m (E.Exp a)
parseLam env = do
  void (symbolic '\\')
  pat <- parsePattern
  void (symbol "->")
  body <- parseExp (ECons (patName pat) env)
  return (E.lam pat body)

parsePattern :: (SwineParsing m) => m E.Pattern
parsePattern = (asum
  [ E.PatBind . E.Binder <$> ident swineIdentStyle
  , E.PatIgnore . E.Binder . T.tail <$> ident swineIgnoreStyle -- remove the _
  ]) <?> "pattern"

parseLet :: (SwineParsing m) => Env a -> m (E.Exp a)
parseLet env = do
  void (swineReserve "let")
  pat <- parsePattern
  void (symbol "<-")
  e1 <- parseExp env
  void (symbolic ';')
  e2 <- parseExp (ECons (patName pat) env)
  return (E.Let pat e1 e2)

parsePrim :: (SwineParsing m) => m E.Prim
parsePrim = (asum
  -- TODO consider throwing errors on out-of-bounds integers
  [ E.PrimInt64 . fromIntegral <$> (swineReserve "i64" >> integer)
  ]) <?> "primitive value"

swineIdent :: (SwineParsing m) => m Text
swineIdent = ident swineIdentStyle

swineReserve :: (SwineParsing m) => Text -> m ()
swineReserve txt = void (reserve swineIdentStyle (T.unpack txt))

swineIdentStyle :: (SwineParsing m) => IdentifierStyle m
swineIdentStyle = IdentifierStyle
  { _styleName = "swine"
  -- We use $ to pretty-print internal stuff, _ for ignore patterns
  , _styleStart = oneOfSet $ CharSet.difference swineIdentCharsStart (CharSet.singleton '_' <> CharSet.singleton '$')
  , _styleLetter = oneOfSet swineIdentCharsLetter
  , _styleReserved = HS.fromList
      [ "case", "let", "->", "<-"
      , "i64"
      ]
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

swineIgnoreStyle :: (SwineParsing m) => IdentifierStyle m
swineIgnoreStyle = swineIdentStyle
  { _styleStart = oneOfSet (CharSet.singleton '_')
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

