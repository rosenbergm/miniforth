module Eval (evaluate, Context) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Parser
import qualified Stack as S
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

type Context = Map.Map String [FExp]

eval :: Context -> [FExp] -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
eval ctx [] stack = Right (Nothing, ctx, stack)
eval ctx (FNum n : xs) stack =
  let newStack = S.push n stack
   in eval ctx xs newStack
eval ctx (FOp op : xs) stack =
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

evaluate :: Context -> String -> S.Stack Integer -> (String, Context, S.Stack Integer)
evaluate ctx input currentStack = unsafePerformIO $ do
  return $ case parse parseExpressions "" input of
    Left err -> (errorBundlePretty err, ctx, currentStack)
    Right expressions ->
      case eval ctx expressions currentStack of
        Left errorMsg -> (errorMsg, ctx, currentStack)
        Right (output, newDict, newStack) ->
          (Maybe.fromMaybe "ok" output, newDict, newStack)
