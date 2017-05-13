{-# LANGUAGE FlexibleContexts #-}
module Swine.Parser
  ( module Text.Parser.Char
  , module Text.Parser.Combinators
  , module Text.Parser.Token
  , module Text.Parser.Token.Highlight
  , SwineParseMonad
  , SwineParsing(..)
  , runSwineParsing
  , symbolic
  , swineIdent
  , swineIgnore
  , swineReserve
  , swinePrim
  , swinePrimType
  , swinePrimOp
  ) where

import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Parser.Token as Parser
import           Text.Parser.Token hiding (symbolic)
import           Text.Parser.Token.Highlight
import           Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import qualified Data.CharSet.Unicode as Unicode
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Trifecta as Trifecta
import qualified Text.Trifecta.Delta as Trifecta

import           Swine.Prelude
import qualified Swine.Pretty as P
import Swine.Prim

class (TokenParsing m, Monad m) => SwineParsing m where
  throwParseError :: P.Pretty err => err -> m a

newtype SwineParseMonad a = SwineParseMonad
  { _unSwineParseMonad :: Trifecta.Parser a }
  deriving (Functor, Applicative, Monad, TokenParsing, CharParsing, Parsing, Alternative)

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

symbolic :: (SwineParsing m) => Char -> m ()
symbolic ch = void (Parser.symbolic ch)

swinePrim :: (SwineParsing m) => m Prim
swinePrim = asum
  [ swineReserve txt >> p
  | (txt, p) <- table
  ]
  where
    table =
      [ ("i64", PrimInt64 . fromIntegral <$> integer) -- TODO check bounds
      ]

swinePrimType :: (SwineParsing m) => m PrimType
swinePrimType = PrimTypeInt64 <$ swineReserve "I64"

swinePrimOp :: (SwineParsing m) => m PrimOp
swinePrimOp = asum
  [ PrimOpInt64Plus <$ swineReserve "I64+"
  , PrimOpInt64Minus <$ swineReserve "I64-"
  ]

swineIgnore :: (SwineParsing m) => m Text
swineIgnore = T.tail <$> ident swineIgnoreStyle

swineIdent :: (SwineParsing m) => m Text
swineIdent = ident swineIdentStyle

swineReserve :: (SwineParsing m) => Text -> m ()
swineReserve txt = void (reserve swineIdentStyle (T.unpack txt))

swineIdentStyle :: (SwineParsing m) => IdentifierStyle m
swineIdentStyle = IdentifierStyle
  { _styleName = "identifier"
  -- We use $ to pretty-print internal stuff, _ for ignore patterns
  , _styleStart = oneOfSet $ CharSet.difference swineIdentCharsStart (CharSet.singleton '_' <> CharSet.singleton '$')
  , _styleLetter = oneOfSet swineIdentCharsLetter
  , _styleReserved = HS.fromList $
      [ "case", "let", "rec", "->", "<-", "="
      , "Type", "?"
      , "I64", "i64", "I64+", "I64-"
      , "TyEq", "ValEq"
      , "coe", "axiom"
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
  (CharSet.fromList ['\'', '\\', '"', '{', '}', '[', ']', '(', ')', ',', ';', ':', '.', '|'])

-- We exclude numbers from the beginning since otherwise we could have
-- variables that look like number literals
swineIdentCharsLetter :: CharSet
swineIdentCharsLetter = swineIdentCharsStart <> Unicode.number

