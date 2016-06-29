module Main where

-- local imports
import Compiler.Parser
-- external imports
import System.Environment
import System.IO
import Control.Monad

process :: String -> IO ()
process line = do
  let res = parseProgram line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

main :: IO ()
main = do
    -- open the example file
    contents <- readFile "example/HelloWorld.yahl"
    process contents
