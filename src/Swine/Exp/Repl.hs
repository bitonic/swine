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

    processInput :: String -> Haskeline.InputT IO ()
    processInput input = do
      let mbExp :: Either P.Doc (E.Syntax Void)
          mbExp = EPa.runSwineParsing "repl" 0 0 (T.encodeUtf8 (T.pack input)) (EPa.parseSyntax (EPa.ENil mempty) <* EPa.eof)
      case mbExp of
        Left err -> do
          outputDoc ("Error while parsing" P.<#> err)
        Right syn -> do
          outputDoc $
            "Parsed expression:" P.<#>
            P.indent (EPr.prettySyntax EPr.PosNormal (EPr.newVarNames absurd) syn)
          case E.eval syn of
            Left _err -> do
              outputDoc "Error while evaluating"
            Right exp -> do
              outputDoc $
                "Evaluated expression:" P.<#>
                P.indent (EPr.prettySyntax EPr.PosNormal (EPr.newVarNames absurd) (E.evalToSyntax exp))
              outputDoc $
                "Evaluated expression without suspensions:" P.<#>
                P.indent (EPr.prettySyntax EPr.PosNormal (EPr.newVarNames absurd) (E.removeAllSusps (E.Syntax (E.evalToSyntax exp))))
      loop

    loop :: Haskeline.InputT IO ()
    loop = do
      mbInput <- Haskeline.getInputLine "SWINE> "
      case mbInput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just ":{" -> do
          let collect :: Bwd String -> Haskeline.InputT IO ()
              collect chunks = do
                mbInput' <- Haskeline.getInputLine "     | "
                case mbInput' of
                  Nothing -> return ()
                  Just "}:" -> processInput (concat (intersperse "\n" (toList chunks)))
                  Just chunk -> collect (chunks :> chunk)
          collect BwdNil
        Just (':' : cmd) -> do
          outputDoc ("Unrecognized command" P.<+> P.text (T.pack cmd))
          loop
        Just input -> processInput input
