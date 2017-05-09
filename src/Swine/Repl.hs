module Swine.Repl where

import qualified System.Console.Haskeline as Haskeline
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Swine.Prelude
import qualified Swine.Pretty as P
import qualified Swine.Parser as EPa

run :: (forall m. (EPa.SwineParsing m) => m a) -> (a -> IO P.Doc) -> IO ()
run parseInput processInput = Haskeline.runInputT
  Haskeline.defaultSettings
    { Haskeline.historyFile = Just ".swine_history" }
  loop
  where
    outputDoc = Haskeline.outputStrLn . P.render

    loop :: Haskeline.InputT IO ()
    loop = do
      mbInput <- Haskeline.getInputLine "SWINE> "
      let processAndLoop s = do
            let mbX = EPa.runSwineParsing "repl" 0 0 (T.encodeUtf8 (T.pack s)) (parseInput <* EPa.eof)
            case mbX of
              Left err -> do
                outputDoc ("Error while parsing" P.<#> err)
                loop
              Right x -> do
                outputDoc =<< liftIO (processInput x)
                loop
      case mbInput of
        Nothing -> return ()
        Just (':' : cmd) -> case words cmd of
            ["q"] -> return ()
            ["{"] -> do
              let collect :: Bwd String -> Haskeline.InputT IO ()
                  collect chunks = do
                    mbInput' <- Haskeline.getInputLine "     | "
                    case mbInput' of
                      Nothing -> return ()
                      Just "}:" -> processAndLoop (concat (intersperse "\n" (toList chunks)))
                      Just chunk -> collect (chunks :> chunk)
              collect BwdNil
            ["l", file] -> processAndLoop =<< liftIO (readFile file)
            _ -> do
              outputDoc ("Unrecognized command" P.<+> P.text (T.pack cmd))
              loop
        Just input -> processAndLoop input
