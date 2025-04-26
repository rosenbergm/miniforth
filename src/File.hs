module File (fetchProgram, runFile) where

import qualified Data.Map as Map
import Data.Maybe
import Eval (Context, evaluate)
import qualified Stack as S
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, openFile)

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
      let (newCtx, result, newStack) = evaluate ctx line stack

      putStrLn $ fromMaybe "ok" result

      processLines newCtx newStack rest

fetchProgram :: FilePath -> IO (String, Handle)
fetchProgram path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle

  return (contents, handle)
