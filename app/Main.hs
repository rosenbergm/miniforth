module Main (main) where

import Repl (repl)

-- debug :: (Show a) => a -> a
-- debug a = trace ("DEBUG: " ++ show a) a

main :: IO ()
main = repl
