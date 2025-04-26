module Eval (evaluate, Context) where

import qualified Data.List as List
import qualified Data.Map as Map
import Parser
import qualified Stack as S
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Util

type Context = Map.Map String [FNode]

evalUnaryOp :: Context -> FUnOperator -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
evalUnaryOp ctx op stack =
  case S.pop stack of
    Just (x, stack') ->
      let result = case op of
            FNeg -> Right $ if x == 0 then -1 else 0
       in case result of
            Left err -> Left err
            Right val -> Right (Nothing, ctx, S.push val stack')
    Nothing -> Left "stack underflow"

evalBinaryOp :: Context -> FBinOperator -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
evalBinaryOp ctx op stack =
  case S.pop stack of
    Just (x1, stack') -> case S.pop stack' of
      Just (x2, stack'') ->
        let result = case op of
              FAdd -> Right $ x2 + x1
              FMul -> Right $ x2 * x1
              FSub -> Right $ x2 - x1
              FDiv ->
                if x1 == 0
                  then Left "division by zero"
                  else Right (x2 `div` x1)
              FMod ->
                if x1 == 0
                  then Left "modulo by zero"
                  else Right (x2 `mod` x1)
              FEq -> Right $ if x2 == x1 then -1 else 0
              FLt -> Right $ if x2 < x1 then -1 else 0
              FGt -> Right $ if x2 > x1 then -1 else 0
              FAnd -> Right $ if x2 /= 0 && x1 /= 0 then -1 else 0
              FOr -> Right $ if x2 /= 0 || x1 /= 0 then -1 else 0
         in case result of
              Left err -> Left err
              Right val -> Right (Nothing, ctx, S.push val stack'')
      Nothing -> Left "stack underflow"
    Nothing -> Left "stack underflow"

evalDirective :: Context -> Directive -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
evalDirective ctx dir stack =
  case dir of
    Dup ->
      case S.peek stack of
        Just x -> Right (Nothing, ctx, S.push x stack)
        Nothing -> Left "stack underflow"
    Drop ->
      case S.pop stack of
        Just (_, stack') -> Right (Nothing, ctx, stack')
        Nothing -> Left "stack underflow"
    Swap ->
      case S.swap stack of
        Just stack' -> Right (Nothing, ctx, stack')
        Nothing -> Left "stack underflow"
    Over ->
      case S.over stack of
        Just stack' -> Right (Nothing, ctx, stack')
        Nothing -> Left "stack underflow"
    Rot ->
      case S.rot stack of
        Just stack' -> Right (Nothing, ctx, stack')
        Nothing -> Left "stack underflow"
    Clear -> Right (Nothing, ctx, S.empty)
    Dot ->
      case S.pop stack of
        Just (val, stack') -> Right (Just $ show val ++ " ", ctx, stack')
        Nothing -> Left "stack underflow"
    DotS -> do
      let stackStr = show (List.reverse $ S.toList stack)
      Right (Just $ stackStr ++ " <- TOP", ctx, stack)
    Emit ->
      case S.pop stack of
        Just (val, stack') -> Right (Just [toEnum (fromIntegral val) :: Char], ctx, stack')
        Nothing -> Left "stack underflow"
    Cr -> Right (Just "\n", ctx, stack)
    DotString str -> Right (Just str, ctx, stack)
    I ->
      case S.peek stack of
        Just _loopIdx -> Right (Nothing, ctx, stack)
        Nothing -> Left "i directive requires a loop index on the stack"

evalWord :: Context -> String -> [String] -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
evalWord ctx name callStack stack =
  if name `elem` callStack
    then Left $ "recursive call detected: " ++ name
    else case Map.lookup name ctx of
      Just body ->
        evalSequence ctx body (name : callStack) stack
      Nothing ->
        Left $
          name ++ " ?"

evalSequence :: Context -> [FNode] -> [String] -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
evalSequence ctx [] _ stack = Right (Nothing, ctx, stack)
evalSequence ctx (node : rest) callStack stack = do
  result <- eval ctx node callStack stack
  case result of
    (output, newCtx, newStack) -> do
      restResult <- evalSequence newCtx rest callStack newStack
      case (output, restResult) of
        (Nothing, result') -> Right result'
        (Just out, (Nothing, finalCtx, finalStack)) ->
          Right (Just out, finalCtx, finalStack)
        (Just out, (Just restOut, finalCtx, finalStack)) ->
          Right (Just (out ++ restOut), finalCtx, finalStack)

evalDoLoop :: Context -> [FNode] -> [String] -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
evalDoLoop ctx loopBody callStack stack =
  case S.pop stack of
    Just (start, stack') ->
      case S.pop stack' of
        Just (end, stack'') ->
          let loopIteration currentI currentStack accOutput =
                if currentI >= end
                  then Right (accOutput, ctx, currentStack)
                  else do
                    let stackWithIndex = S.push currentI currentStack

                    result <- evalSequence ctx loopBody callStack stackWithIndex

                    case result of
                      (iterOutput, _newCtx, newStack) ->
                        let combinedOutput =
                              case (accOutput, iterOutput) of
                                (Nothing, Nothing) -> Nothing
                                (Just out, Nothing) -> Just out
                                (Nothing, Just iterOut) -> Just iterOut
                                (Just out, Just iterOut) -> Just (out ++ iterOut)
                         in loopIteration (currentI + 1) newStack combinedOutput
           in loopIteration start stack'' Nothing
        Nothing -> Left "stack underflow in do-loop"
    Nothing -> Left "stack underflow in do-loop"

eval :: Context -> FNode -> [String] -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
eval ctx (Literal expr) cs stack =
  case expr of
    FNum num -> Right (Nothing, ctx, S.push num stack)
    FUnOp op -> evalUnaryOp ctx op stack
    FBinOp op -> evalBinaryOp ctx op stack
    FDirective dir -> evalDirective ctx dir stack
    FWord name -> evalWord ctx name cs stack
eval ctx (WordDef (Definition name body)) cs stack = do
  Right (Nothing, Map.insert name body ctx, stack)
eval ctx (Sequence nodes) cs stack = evalSequence ctx nodes cs stack
eval ctx (IfThenElse thenBranch elseBranch) callStack stack =
  case S.pop stack of
    Just (val, stack') ->
      if val /= 0
        then
          evalSequence ctx thenBranch callStack stack'
        else
          evalSequence ctx elseBranch callStack stack'
    Nothing -> Left "stack underflow in if-then-else"
eval ctx (DoLoop body) cs stack =
  evalDoLoop ctx body cs stack

evaluate :: Context -> String -> S.Stack Integer -> (Context, Maybe String, S.Stack Integer)
evaluate ctx input currentStack =
  case parse parseExpressions "" input of
    Left err -> (ctx, Just $ errorBundlePretty err, currentStack)
    Right expressions ->
      case eval ctx (debug expressions) [] currentStack of
        Left errorMsg -> (ctx, Just errorMsg, currentStack)
        Right (output, newDict, newStack) ->
          (newDict, output, newStack)
