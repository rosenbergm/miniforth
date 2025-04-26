module File (fetchProgram, runFile) where

import Context
import Data.Maybe
import Eval (evaluate)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, openFile)

runFile :: FilePath -> IO ()
runFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle

  let (_ctx, output) = evaluate Context.empty contents

  putStrLn $ fromMaybe "ok" output

  hClose handle

fetchProgram :: FilePath -> IO (String, Handle)
fetchProgram path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle

  return (contents, handle)
