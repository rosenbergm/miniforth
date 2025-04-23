module File where

import Control.Monad (foldM_, unless)
import Eval (evaluate)
import qualified Stack as S
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

runFile :: FilePath -> IO ()
runFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle

  let programLines = lines contents

  foldM_ processLine S.empty programLines

  hClose handle
  where
    processLine :: S.Stack Integer -> String -> IO (S.Stack Integer)
    processLine stack line = do
      let (result, newStack) = evaluate line stack
      unless (result == "ok") $
        putStrLn
          result
      return newStack
