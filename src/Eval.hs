module Eval (evaluate, Context) where

import qualified Data.List as List
import qualified Data.Map as Map
import Parser
import qualified Stack as S
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

type Context = Map.Map String [FExp]

eval :: Context -> [FExp] -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
eval ctx [] stack = Right (Nothing, ctx, stack)
eval ctx (FNum n : xs) stack =
  let newStack = S.push n stack
   in eval ctx xs newStack
eval ctx (FUnOp op : xs) stack =
  case S.pop stack of
    Just (x, stack') ->
      let result = case op of
            FNeg -> Right $ if x == 0 then -1 else 0
       in case result of
            Left err -> Left err
            Right val -> eval ctx xs (S.push val stack')
    Nothing -> Left "stack underflow"
eval ctx (FBinOp op : xs) stack =
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
              FEq -> Right $ if x2 == x1 then -1 else 0
              FLt -> Right $ if x2 < x1 then -1 else 0
              FGt -> Right $ if x2 > x1 then -1 else 0
              FAnd -> Right $ if x2 /= 0 && x1 /= 0 then -1 else 0
              FOr -> Right $ if x2 /= 0 || x1 /= 0 then -1 else 0
         in case result of
              Left err -> Left err
              Right val -> eval ctx xs (S.push val stack'')
      Nothing -> Left "stack underflow"
    Nothing -> Left "stack underflow"
eval ctx (FDirective dir : xs) stack =
  case dir of
    Dup ->
      case S.peek stack of
        Just x -> eval ctx xs (S.push x stack)
        Nothing -> Left "stack underflow"
    Drop ->
      case S.pop stack of
        Just (_, stack') -> eval ctx xs stack'
        Nothing -> Left "stack underflow"
    Swap ->
      case S.swap stack of
        Just stack' -> eval ctx xs stack'
        Nothing -> Left "stack underflow"
    Over ->
      case S.over stack of
        Just stack' -> eval ctx xs stack'
        Nothing -> Left "stack underflow"
    Rot ->
      case S.rot stack of
        Just stack' -> eval ctx xs stack'
        Nothing -> Left "stack underflow"
    Clear -> eval ctx xs S.empty
    Dot ->
      case S.pop stack of
        Just (val, stack') -> do
          result <- eval ctx xs stack'
          case result of
            (Nothing, finalCtx, finalStack) -> Right (Just $ show val, finalCtx, finalStack)
            (Just output, finalCtx, finalStack) -> Right (Just $ show val ++ " " ++ output, finalCtx, finalStack)
        Nothing -> Left "stack underflow"
    DotS -> do
      let stackStr = show (List.reverse $ S.toList stack)
      result <- eval ctx xs stack
      case result of
        (Nothing, finalDict, finalStack) ->
          Right (Just $ stackStr ++ " <- TOP", finalDict, finalStack)
        (Just output, finalDict, finalStack) ->
          Right (Just $ stackStr ++ " <- TOP\n" ++ output, finalDict, finalStack)
    Emit ->
      case S.pop stack of
        Just (val, stack') -> do
          result <- eval ctx xs stack'
          case result of
            (Nothing, finalCtx, finalStack) -> Right (Just [toEnum (fromIntegral val) :: Char], finalCtx, finalStack)
            (Just output, finalCtx, finalStack) -> Right (Just $ (toEnum (fromIntegral val) :: Char) : output, finalCtx, finalStack)
        Nothing -> Left "stack underflow"
    Cr -> do
      result <- eval ctx xs stack
      case result of
        (output, finalCtx, finalStack) -> Right (fmap ("\n" ++) output, finalCtx, finalStack)
    DotString str -> do
      result <- eval ctx xs stack
      case result of
        (Nothing, finalCtx, finalStack) -> Right (Just str, finalCtx, finalStack)
        (Just output, finalCtx, finalStack) -> Right (Just $ str ++ output, finalCtx, finalStack)
eval ctx (FDefine (Definition name body) : xs) stack = do
  let ctx' = Map.insert name body ctx
  eval ctx' xs stack
eval ctx (FWord name : xs) stack = do
  case Map.lookup name ctx of
    Just body -> do
      result <- eval ctx body stack
      case result of
        (output, newDict, newStack) -> do
          restResult <- eval newDict xs newStack
          case (output, restResult) of
            (Nothing, (restOutput, finalDict, finalStack)) ->
              Right (restOutput, finalDict, finalStack)
            (Just out, (Nothing, finalDict, finalStack)) ->
              Right (Just out, finalDict, finalStack)
            (Just out, (Just restOut, finalDict, finalStack)) ->
              Right (Just (out ++ restOut), finalDict, finalStack)
    Nothing -> Left $ name ++ " ?"

evaluate :: Context -> String -> S.Stack Integer -> (Context, Maybe String, S.Stack Integer)
evaluate ctx input currentStack =
  case parse parseExpressions "" input of
    Left err -> (ctx, Just $ errorBundlePretty err, currentStack)
    Right expressions ->
      case eval ctx expressions currentStack of
        Left errorMsg -> (ctx, Just errorMsg, currentStack)
        Right (output, newDict, newStack) ->
          (newDict, output, newStack)
