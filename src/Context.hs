module Context where

import qualified Data.Map as Map
import Parser (FNode)
import qualified Stack as S

data Context = Context
  { valueStack :: S.Stack Integer,
    definedWords :: Map.Map String [FNode]
  }
  deriving (Show)

empty :: Context
empty = Context S.empty Map.empty

mapStack :: (S.Stack Integer -> S.Stack Integer) -> Context -> Context
mapStack f ctx = ctx {valueStack = f (valueStack ctx)}

mapWords :: (Map.Map String [FNode] -> Map.Map String [FNode]) -> Context -> Context
mapWords f ctx = ctx {definedWords = f (definedWords ctx)}

withStack :: S.Stack Integer -> Context -> Context
withStack stack ctx = ctx {valueStack = stack}

withWords :: Map.Map String [FNode] -> Context -> Context
withWords words' ctx = ctx {definedWords = words'}
