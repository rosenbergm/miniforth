module Repl where

import Control.Monad.State.Strict
import qualified Data.List as List
import Eval (evaluate)
import qualified Stack as S
import System.Console.Haskeline

forthCompletion :: CompletionFunc IO
forthCompletion = completeWord Nothing [] forthCompleter
  where
    forthCompleter :: String -> IO [Completion]
    forthCompleter str = do
      let words = ["dup", "drop", "swap", "over", "rot", "clear", ".", ".s", "emit", "cr", ".\""]
          matches = filter (List.isPrefixOf str) words
      return $ map simpleCompletion matches

settings :: Settings IO
settings =
  Settings
    { complete = forthCompletion,
      historyFile = Just ".miniforth-history",
      autoAddHistory = True
    }

repl :: IO ()
repl = evalStateT replLoop S.empty
  where
    replLoop :: StateT (S.Stack Integer) IO ()
    replLoop = do
      liftIO $ putStrLn "\nthis is miniforth. type :help for help or :q to exit."

      replWithStack

    replWithStack :: StateT (S.Stack Integer) IO ()
    replWithStack = do
      stack <- get

      input <- liftIO $ runInputT settings $ getInputLine ">>> "

      case input of
        Nothing -> return ()
        Just ":q" -> return ()
        Just ":quit" -> return ()
        Just ":help" -> do
          liftIO $
            putStrLn
              "available commands:\n\
              \  :help - display this help message\n\
              \  :q, :quit - exit the interpreter\n\
              \  :clear - clear the stack"
          replWithStack
        Just ":clear" -> do
          put S.empty
          liftIO $ putStrLn "stack cleared"
          replWithStack
        Just line -> do
          let (result, newStack) = evaluate line stack
          put newStack
          liftIO $ putStrLn result
          replWithStack
