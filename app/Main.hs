module Main (main) where

import File (runFile)
import Repl (repl)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [] -> repl
    ["-h"] -> showHelp
    ["--help"] -> showHelp
    [filePath] -> runFile filePath
    _ -> do
      putStrLn "error: too many arguments."
      showHelp
      exitFailure

showHelp :: IO ()
showHelp = do
  putStrLn "usage: miniforth [options] [file]"
  putStrLn "options:"
  putStrLn "  -h, --help    show this help message"
  putStrLn "file:          path to a file containing Forth code"
  putStrLn "If no file is provided, the interpreter will start in interactive mode."
