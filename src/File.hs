module File where

import Control.Monad (unless)
import qualified Data.Map as Map
import Eval (Context, evaluate)
import qualified Stack as S
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

runFile :: FilePath -> IO ()
runFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle

  let programLines = lines contents

  processLines Map.empty S.empty programLines

  hClose handle
  where
    processLines :: Context -> S.Stack Integer -> [String] -> IO ()
    processLines _ _ [] = return ()
    processLines ctx stack (line : rest) = do
      let (result, newCtx, newStack) = evaluate ctx line stack

      unless (result == "ok") $
        putStrLn result

      processLines newCtx newStack rest
