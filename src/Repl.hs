module Repl where

import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.State.Strict
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Eval (Context, evaluate)
import qualified Stack as S
import System.Console.Haskeline

type ForthM a = ReaderT Context (StateT (S.Stack Integer) IO) a

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
repl = evalStateT (runReaderT replLoop Map.empty) S.empty
  where
    replLoop :: ForthM ()
    replLoop = do
      liftIO $ putStrLn "\nthis is miniforth. type :help for help or :q to exit."

      replWithStack

    replWithStack :: ForthM ()
    replWithStack = do
      stack <- lift get
      ctx <- ask

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
          lift $ put S.empty
          liftIO $ putStrLn "stack cleared"
          replWithStack
        Just ":words" -> do
          ctx' <- ask
          liftIO $ putStrLn $ "Defined words: " ++ show (Map.keys ctx')
          replWithStack
        Just line -> do
          let (newCtx, result, newStack) = evaluate ctx line stack
          lift $ put newStack
          liftIO $ putStrLn $ Maybe.fromMaybe "ok" result
          local (const newCtx) replWithStack
