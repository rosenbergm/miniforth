module Repl (readInput) where

import System.IO (hFlush, stdout)

readInput :: IO String
readInput = putStr "> " >> hFlush stdout >> getLine

-- run :: IO ()
-- run =