module Main (main) where

import Control.Monad (unless)
import qualified Data.List as List
import Data.Maybe
import Debug.Trace
import Parser (Directive (..), FExp (..), FOperator (..), parseExpressions)
import qualified Repl
import qualified Stack as S
import Text.Megaparsec (errorBundlePretty, parse)

debug :: (Show a) => a -> a
debug a = trace ("DEBUG: " ++ show a) a

eval :: [FExp] -> S.Stack Integer -> Either String (Maybe String, S.Stack Integer)
eval [] stack = Right (Nothing, stack)
eval (FNum n : xs) stack =
  let newStack = S.push n stack
   in eval xs newStack
eval (FOp op : xs) stack =
  case S.pop stack of
    Just (x1, stack') -> case S.pop stack' of
      Just (x2, stack'') ->
        let result = case op of
              FAdd -> Right $ x2 + x1
              FMul -> Right $ x2 * x1
              FSub -> Right $ x2 - x1
              FDiv ->
                if x1 == 0
                  then Left "Division by zero"
                  else Right (x2 `div` x1)
         in case result of
              Left err -> Left err
              Right val -> eval xs (S.push val stack'')
      Nothing -> Left "stack underflow"
    Nothing -> Left "stack underflow"
eval (FDirective dir : xs) stack =
  case dir of
    Dup ->
      case S.peek stack of
        Just x -> eval xs (S.push x stack)
        Nothing -> Left "stack underflow"
    Drop ->
      case S.pop stack of
        Just (_, stack') -> eval xs stack'
        Nothing -> Left "stack underflow"
    Swap ->
      case S.swap stack of
        Just stack' -> eval xs stack'
        Nothing -> Left "stack underflow"
    Over ->
      case S.over stack of
        Just stack' -> eval xs stack'
        Nothing -> Left "stack underflow"
    Rot ->
      case S.rot stack of
        Just stack' -> eval xs stack'
        Nothing -> Left "stack underflow"
    Clear -> eval xs S.empty
    Dot ->
      case S.pop stack of
        Just (val, stack') -> do
          result <- eval xs stack'
          case result of
            (Nothing, finalStack) -> Right (Just $ show val, finalStack)
            (Just output, finalStack) -> Right (Just $ show val ++ " " ++ output, finalStack)
        Nothing -> Left "stack underflow"
    DotS -> do
      let stackStr = show (List.reverse $ S.toList stack)
      Right (Just $ stackStr ++ " <- TOP", stack)
    Emit ->
      case S.pop stack of
        Just (val, stack') -> do
          result <- eval xs stack'
          case result of
            (Nothing, finalStack) -> Right (Just [toEnum (fromIntegral val) :: Char], finalStack)
            (Just output, finalStack) -> Right (Just $ (toEnum (fromIntegral val) :: Char) : output, finalStack)
        Nothing -> Left "stack underflow"
    Cr -> do
      result <- eval xs stack
      case result of
        (output, finalStack) -> Right (fmap ("\n" ++) output, finalStack)
    DotString str -> do
      result <- eval xs stack
      case result of
        (Nothing, finalStack) -> Right (Just str, finalStack)
        (Just output, finalStack) -> Right (Just $ str ++ output, finalStack)

evaluate :: String -> S.Stack Integer -> (String, S.Stack Integer)
evaluate input currentStack =
  case parse parseExpressions "" input of
    Left err -> (errorBundlePretty err, currentStack)
    Right expressions ->
      case eval (debug expressions) currentStack of
        Left errorMsg -> (errorMsg, currentStack)
        Right (output, newStack) ->
          (fromMaybe "ok" output, newStack)

replWithStack :: S.Stack Integer -> IO ()
replWithStack stack = do
  input <- Repl.readInput

  unless (input == ":q") $ do
    let (result, newStack) = evaluate input stack
    putStrLn result
    replWithStack newStack

main :: IO ()
main = replWithStack S.empty
