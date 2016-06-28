module Main where

-- local imports
import Compiler.Parser
-- external imports
import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment

process :: String -> IO ()
process line = do
  let res = parseProgram line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "yahl> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop