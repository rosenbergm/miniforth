module Eval (evaluate, Context) where

import Context
import qualified Data.List as List
import qualified Data.Map as Map
import Parser
import qualified Stack as S
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Util (debug)

data Output
  = None
  | Message String
  | Quit
  | WordExit (Maybe String)

type EvalResult = Either String (Output, Context)

evalUnaryOp :: Context.Context -> FUnOperator -> EvalResult
evalUnaryOp ctx op =
  case S.pop $ valueStack ctx of
    Just (x, stack') ->
      let result = case op of
            FNeg -> Right $ if x == 0 then -1 else 0
            FInvert -> Right (-x)
       in case result of
            Left err -> Left err
            Right val -> Right (None, flip withStack ctx $ S.push val stack')
    Nothing -> Left "stack underflow"

evalBinaryOp :: Context -> FBinOperator -> EvalResult
evalBinaryOp ctx op = do
  case S.pop2 stack of
    Nothing -> Left "stack underflow"
    Just (x1, x2, stack') ->
      case op of
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
        >>= \value -> Right (None, flip withStack ctx $ S.push value stack')
  where
    stack = valueStack ctx

evalDirective :: Context -> Directive -> EvalResult
evalDirective ctx dir =
  case dir of
    Dup ->
      case S.peek stack of
        Just x -> Right (None, flip withStack ctx $ S.push x stack)
        Nothing -> Left "stack underflow"
    Drop ->
      case S.pop stack of
        Just (_, stack') -> Right (None, withStack stack' ctx)
        Nothing -> Left "stack underflow"
    Swap ->
      case S.swap stack of
        Just stack' -> Right (None, withStack stack' ctx)
        Nothing -> Left "stack underflow"
    Over ->
      case S.over stack of
        Just stack' -> Right (None, withStack stack' ctx)
        Nothing -> Left "stack underflow"
    Rot ->
      case S.rot stack of
        Just stack' -> Right (None, withStack stack' ctx)
        Nothing -> Left "stack underflow"
    Clear -> Right (None, withStack S.empty ctx)
    Dot ->
      case S.pop stack of
        Just (val, stack') -> Right (Message $ show val ++ " ", withStack stack' ctx)
        Nothing -> Left "stack underflow"
    DotS -> do
      let stackStr = show (List.reverse $ S.toList stack)
      Right (Message $ stackStr ++ " <- TOP", ctx)
    Emit ->
      case S.pop stack of
        Just (val, stack') -> Right (Message [toEnum (fromIntegral val) :: Char], withStack stack' ctx)
        Nothing -> Left "stack underflow"
    Cr -> Right (Message "\n", ctx)
    DotString str -> Right (Message str, ctx)
    Exit -> Right (WordExit Nothing, ctx)
  where
    stack = valueStack ctx

evalWord :: Context -> String -> [String] -> EvalResult
evalWord ctx name callStack =
  if name `elem` callStack
    then Left $ "recursive call detected: " ++ name
    else case Map.lookup name $ definedWords ctx of
      Just body -> do
        result <- evalSequence ctx body (name : callStack)
        case result of
          (WordExit msg, newCtx) ->
            case msg of
              Just str -> Right (Message str, newCtx)
              Nothing -> Right (None, newCtx)
          _ -> Right result
      Nothing ->
        Left $
          name ++ " ?"

evalSequence :: Context -> [FNode] -> [String] -> EvalResult
evalSequence ctx [] _ = Right (None, ctx)
evalSequence ctx (node : rest) callStack = do
  result <- eval ctx node callStack
  case result of
    (output, newCtx) -> do
      case output of
        Quit -> Right (Quit, newCtx)
        WordExit msg -> Right (WordExit msg, newCtx)
        _ -> do
          restResult <- evalSequence newCtx rest callStack
          case (output, restResult) of
            (None, result') -> Right result'
            (Message out, (None, finalCtx)) ->
              Right (Message out, finalCtx)
            (Message out, (Message restOut, finalCtx)) ->
              Right (Message (out ++ restOut), finalCtx)
            (Message out, (WordExit Nothing, finalCtx)) ->
              Right (WordExit (Just out), finalCtx)
            (Message out, (WordExit (Just exitMsg), finalCtx)) ->
              Right (WordExit (Just (out ++ exitMsg)), finalCtx)
            (_, (Quit, finalCtx)) -> Right (Quit, finalCtx)

evalDoLoop :: Context -> [FNode] -> [String] -> EvalResult
evalDoLoop ctx loopBody callStack =
  case S.pop2 stack of
    Nothing -> Left "stack underflow in do-loop"
    Just (start, end, stack') ->
      let loopCtx = withStack stack' ctx
          loopIteration currentI currentCtx accOutput =
            if currentI >= end
              then Right (accOutput, currentCtx)
              else do
                let ctxWithIndex = flip withStack currentCtx $ S.push currentI (valueStack currentCtx)
                result <- evalSequence ctxWithIndex loopBody callStack

                case result of
                  (Quit, newCtx) -> Right (Quit, newCtx)
                  (iterOutput, newCtx) ->
                    let combinedOutput =
                          case (accOutput, iterOutput) of
                            (None, None) -> None
                            (Message out, None) -> Message out
                            (None, Message iterOut) -> Message iterOut
                            (Message out, Message iterOut) -> Message (out ++ iterOut)
                            _ -> Quit
                     in loopIteration (currentI + 1) newCtx combinedOutput
       in loopIteration start loopCtx None
  where
    stack = valueStack ctx

evalBeginAgain :: Context -> [FNode] -> [String] -> EvalResult
evalBeginAgain ctx loopBody callStack =
  let loopIteration currentCtx accMsg =
        case evalSequence currentCtx loopBody callStack of
          Left err -> Left err
          Right (WordExit (Just newMsg), newCtx) ->
            Right (Message (accMsg ++ newMsg), newCtx)
          Right (WordExit Nothing, newCtx) ->
            if null accMsg
              then Right (None, newCtx)
              else Right (Message accMsg, newCtx)
          Right (Quit, newCtx) ->
            if null accMsg
              then Right (Quit, newCtx)
              else Right (Message accMsg, newCtx)
          Right (Message newMsg, newCtx) ->
            loopIteration newCtx (accMsg ++ newMsg)
          Right (None, newCtx) ->
            loopIteration newCtx accMsg
   in loopIteration ctx ""

eval :: Context -> FNode -> [String] -> EvalResult
eval ctx (Literal expr) cs =
  case expr of
    FNum num -> Right (None, flip withStack ctx $ S.push num stack)
    FUnOp op -> evalUnaryOp ctx op
    FBinOp op -> evalBinaryOp ctx op
    FDirective dir -> evalDirective ctx dir
    FWord name -> evalWord ctx name cs
  where
    stack = valueStack ctx
eval ctx (WordDef (Definition name body)) _cs = do
  Right (None, mapWords (Map.insert name body) ctx)
eval ctx (Sequence nodes) cs = evalSequence ctx nodes cs
eval ctx (IfThenElse thenBranch elseBranch) callStack =
  case S.pop stack of
    Just (val, stack') ->
      if val /= 0
        then
          evalSequence (withStack stack' ctx) thenBranch callStack
        else
          evalSequence (withStack stack' ctx) elseBranch callStack
    Nothing -> Left "stack underflow in if-then-else"
  where
    stack = valueStack ctx
eval ctx (DoLoop body) cs =
  evalDoLoop ctx body cs
eval ctx (BeginAgain body) cs =
  evalBeginAgain ctx body cs

evaluate :: Context -> String -> (Context, Maybe String)
evaluate ctx input =
  case parse parseExpressions "" input of
    Left err -> (ctx, Just $ errorBundlePretty err)
    Right expressions ->
      case eval ctx expressions [] of
        Left errorMsg -> (ctx, Just errorMsg)
        Right (output, newDict) ->
          case output of
            None -> (newDict, Nothing)
            Message msg -> (newDict, Just msg)
            Quit -> (newDict, Nothing)
            WordExit msg -> (newDict, msg)
