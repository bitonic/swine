module Swine.Surface.Repl (run) where

import qualified Swine.Repl as Repl
import           Swine.Surface.Parser
import           Swine.Surface.Pretty
import           Swine.Pretty
import           Swine.Prelude

run :: IO ()
run = Repl.run parseExp $ \exp -> return $ vsep
  [ "Parsed expression:"
  , indent (prettyExp PosNormal exp)
  ]
