module Eval (evaluate, Context) where

import qualified Data.List as List
import qualified Data.Map as Map
import Parser
import qualified Stack as S
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Util

type Context = Map.Map String [FNode]

skipToThen :: [FExp] -> Either String [FExp]
skipToThen [] = Left "Missing 'then'"
skipToThen (FDirective Then : rest) = Right rest
skipToThen (_ : xs) = skipToThen xs

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
        Just (val, stack') -> Right (Just $ show val, ctx, stack')
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
    If -> Left "if directive should be handled as an IfThenElse node"
    Else -> Left "else directive should be handled as an IfThenElse node"
    Then -> Left "then directive should be handled as part of an IfThenElse node"

evalWord :: Context -> String -> [String] -> S.Stack Integer -> Either String (Maybe String, Context, S.Stack Integer)
evalWord ctx name callStack stack =
  if name `elem` callStack
    then Left $ "recursive call detected: " ++ name
    else case Map.lookup name ctx of
      Just body ->
        -- Add the current word to the call stack to detect recursion
        evalSequence ctx body (name : callStack) stack
      Nothing -> Left $ name ++ " ?"

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
eval ctx (IfThenElse _ thenBranch elseBranch) callStack stack =
  case S.pop stack of
    Just (val, stack') ->
      if val /= 0
        then -- Condition is true, execute then branch
          evalSequence ctx thenBranch callStack stack'
        else -- Condition is false, execute else branch
          evalSequence ctx elseBranch callStack stack'
    Nothing -> Left "stack underflow in if-then-else"

-- eval ctx (FUnOp op : xs) stack =
--   case S.pop stack of
--     Just (x, stack') ->
--       let result = case op of
--             FNeg -> Right $ if x == 0 then -1 else 0
--        in case result of
--             Left err -> Left err
--             Right val -> eval ctx xs (S.push val stack')
--     Nothing -> Left "stack underflow"
-- eval ctx (FBinOp op : xs) stack =
--   case S.pop stack of
--     Just (x1, stack') -> case S.pop stack' of
--       Just (x2, stack'') ->
--         let result = case op of
--               FAdd -> Right $ x2 + x1
--               FMul -> Right $ x2 * x1
--               FSub -> Right $ x2 - x1
--               FDiv ->
--                 if x1 == 0
--                   then Left "division by zero"
--                   else Right (x2 `div` x1)
--               FMod ->
--                 if x1 == 0
--                   then Left "division by zero"
--                   else Right (x2 `mod` x1)
--               FEq -> Right $ if x2 == x1 then -1 else 0
--               FLt -> Right $ if x2 < x1 then -1 else 0
--               FGt -> Right $ if x2 > x1 then -1 else 0
--               FAnd -> Right $ if x2 /= 0 && x1 /= 0 then -1 else 0
--               FOr -> Right $ if x2 /= 0 || x1 /= 0 then -1 else 0
--          in case result of
--               Left err -> Left err
--               Right val -> eval ctx xs (S.push val stack'')
--       Nothing -> Left "stack underflow"
--     Nothing -> Left "stack underflow"
-- eval ctx (FDirective dir : xs) stack =
--   case dir of
--     Dup ->
--       case S.peek stack of
--         Just x -> eval ctx xs (S.push x stack)
--         Nothing -> Left "stack underflow"
--     Drop ->
--       case S.pop stack of
--         Just (_, stack') -> eval ctx xs stack'
--         Nothing -> Left "stack underflow"
--     Swap ->
--       case S.swap stack of
--         Just stack' -> eval ctx xs stack'
--         Nothing -> Left "stack underflow"
--     Over ->
--       case S.over stack of
--         Just stack' -> eval ctx xs stack'
--         Nothing -> Left "stack underflow"
--     Rot ->
--       case S.rot stack of
--         Just stack' -> eval ctx xs stack'
--         Nothing -> Left "stack underflow"
--     Clear -> eval ctx xs S.empty
--     Dot ->
--       case S.pop stack of
--         Just (val, stack') -> do
--           result <- eval ctx xs stack'
--           case result of
--             (Nothing, finalCtx, finalStack) -> Right (Just $ show val, finalCtx, finalStack)
--             (Just output, finalCtx, finalStack) -> Right (Just $ show val ++ " " ++ output, finalCtx, finalStack)
--         Nothing -> Left "stack underflow"
--     DotS -> do
--       let stackStr = show (List.reverse $ S.toList stack)
--       result <- eval ctx xs stack
--       case result of
--         (Nothing, finalDict, finalStack) ->
--           Right (Just $ stackStr ++ " <- TOP", finalDict, finalStack)
--         (Just output, finalDict, finalStack) ->
--           Right (Just $ stackStr ++ " <- TOP\n" ++ output, finalDict, finalStack)
--     Emit ->
--       case S.pop stack of
--         Just (val, stack') -> do
--           result <- eval ctx xs stack'
--           case result of
--             (Nothing, finalCtx, finalStack) -> Right (Just [toEnum (fromIntegral val) :: Char], finalCtx, finalStack)
--             (Just output, finalCtx, finalStack) -> Right (Just $ (toEnum (fromIntegral val) :: Char) : output, finalCtx, finalStack)
--         Nothing -> Left "stack underflow"
--     Cr -> do
--       result <- eval ctx xs stack
--       case result of
--         (output, finalCtx, finalStack) -> Right (fmap ("\n" ++) output, finalCtx, finalStack)
--     DotString str -> do
--       result <- eval ctx xs stack
--       case result of
--         (Nothing, finalCtx, finalStack) -> Right (Just str, finalCtx, finalStack)
--         (Just output, finalCtx, finalStack) -> Right (Just $ str ++ output, finalCtx, finalStack)
--     If ->
--       case S.pop stack of
--         Just (cond, stack') ->
--           if cond /= 0
--             then eval ctx xs stack'
--             else case skipToThen xs of
--               Left err -> Left err
--               Right afterThen -> eval ctx afterThen stack'
--         Nothing -> Left "stack underflow"
--     Then -> eval ctx xs stack
-- eval ctx (FDefine (Definition name body) : xs) stack = do
--   let ctx' = Map.insert name body ctx
--   eval ctx' xs stack
-- eval ctx (FWord name : xs) stack = do
--   case Map.lookup name ctx of
--     Just body -> do
--       result <- eval ctx body stack
--       case result of
--         (output, newDict, newStack) -> do
--           restResult <- eval newDict xs newStack
--           case (output, restResult) of
--             (Nothing, (restOutput, finalDict, finalStack)) ->
--               Right (restOutput, finalDict, finalStack)
--             (Just out, (Nothing, finalDict, finalStack)) ->
--               Right (Just out, finalDict, finalStack)
--             (Just out, (Just restOut, finalDict, finalStack)) ->
--               Right (Just (out ++ restOut), finalDict, finalStack)
--     Nothing -> Left $ name ++ " ?"

evaluate :: Context -> String -> S.Stack Integer -> (Context, Maybe String, S.Stack Integer)
evaluate ctx input currentStack =
  case parse parseExpressions "" input of
    Left err -> (ctx, Just $ errorBundlePretty err, currentStack)
    Right expressions ->
      case eval ctx (debug expressions) [] currentStack of
        Left errorMsg -> (ctx, Just errorMsg, currentStack)
        Right (output, newDict, newStack) ->
          (newDict, output, newStack)
