module Util (debug) where

import Debug.Trace

debug :: (Show a) => a -> a
debug a = trace ("DEBUG: " ++ show a) a
