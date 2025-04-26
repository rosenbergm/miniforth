module Util (debug, debugMsg) where

import Debug.Trace

debug :: (Show a) => a -> a
debug a = trace ("DEBUG: " ++ show a) a

debugMsg :: (Show a) => String -> a -> a
debugMsg msg a = trace (msg ++ show a) a
