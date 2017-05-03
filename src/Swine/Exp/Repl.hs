module Swine.Exp.Repl where

import qualified System.Console.Haskeline as Haskeline
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Swine.Prelude
import qualified Swine.Exp as E
import qualified Swine.Exp.Pretty as EPr
import qualified Swine.Exp.Parser as EPa
import qualified Swine.Pretty as P

run :: IO ()
run = Haskeline.runInputT
  Haskeline.defaultSettings
    { Haskeline.historyFile = Just ".swine_history" }
  loop
  where
    outputDoc = Haskeline.outputStrLn . P.render

    loop :: Haskeline.InputT IO ()
    loop = do
      mbInput <- Haskeline.getInputLine "SWINE> "
      case mbInput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          let mbExp :: Either P.Doc (E.Exp Void)
              mbExp = EPa.runSwineParsing "repl" 0 0 (T.encodeUtf8 (T.pack input)) (EPa.parseExp (EPa.ENil mempty) <* EPa.eof)
          case mbExp of
            Left err -> do
              outputDoc ("Error while parsing" P.<#> err)
            Right exp0 -> do
              outputDoc $
                "Parsed expression:" P.<#>
                P.indent (EPr.prettyExp (EPr.newVarNames absurd) exp0)
              case E.eval exp0 of
                Left _err -> do
                  outputDoc "Error while evaluating"
                Right exp -> do
                  outputDoc $
                    "Evaluated expression:" P.<#>
                    P.indent (EPr.prettyExp (EPr.newVarNames absurd) (E.Evaluated exp))
                  case E.removeAllSusps (E.Evaluated exp) of
                    Left _err -> do
                      outputDoc "Error while removing all suspensions"
                    Right exp' -> do
                      outputDoc $
                        "Evaluated expression without suspensions:" P.<#>
                        P.indent (EPr.prettyExp (EPr.newVarNames absurd) exp')
          loop
