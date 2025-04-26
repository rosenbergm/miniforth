module Repl where

import Control.Exception (Exception (..), catch)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.State.Strict
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Eval (Context, evaluate)
import File (fetchProgram)
import Parser (reservedKeywords)
import qualified Stack as S
import System.Console.Haskeline
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)

type ForthM a = ReaderT Context (StateT (S.Stack Integer) IO) a

forthCompletion :: CompletionFunc IO
forthCompletion = completeWord Nothing [] forthCompleter
  where
    forthCompleter :: String -> IO [Completion]
    forthCompleter str = do
      let matches = filter (List.isPrefixOf str) reservedKeywords
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

    parseCommand :: String -> (String, [String])
    parseCommand input =
      let words' = words input
       in case words' of
            [] -> ("", [])
            (cmd : args) -> (cmd, args)

    replWithStack :: ForthM ()
    replWithStack = do
      stack <- lift get
      ctx <- ask

      input <- liftIO $ runInputT settings $ getInputLine ">>> "

      case input of
        Nothing -> return ()
        Just cmd -> case parseCommand cmd of
          (":load", args) -> loadFile args ctx stack
          (":l", args) -> loadFile args ctx stack
          (":help", _) -> do
            liftIO $
              putStrLn
                "available commands:\n\
                \  :help - display this help message\n\
                \  :q, :quit - exit the interpreter\n\
                \  :clear - clear the stack"

            replWithStack
          (":q", _) -> return ()
          (":quit", _) -> return ()
          (":clear", _) -> do
            lift $ put S.empty
            liftIO $ putStrLn "stack cleared"

            replWithStack
          (":words", _) -> do
            ctx' <- ask
            liftIO $ putStrLn $ "defined words: " ++ show (Map.keys ctx')

            replWithStack
          (_, _) -> do
            let (newCtx, result, newStack) = evaluate ctx cmd stack
            lift $ put newStack
            liftIO $ putStrLn $ Maybe.fromMaybe "ok" result
            local (const newCtx) replWithStack

    loadFile :: [String] -> Context -> S.Stack Integer -> ForthM ()
    loadFile args ctx stack = do
      case args of
        [] -> do
          liftIO $ putStrLn "usage: :load <path> or :l <path>"
          replWithStack
        (path : _) -> do
          liftIO $ putStrLn $ "loading file " ++ path
          result <-
            liftIO $
              catch
                ( do
                    (file, handle) <- fetchProgram path
                    return $ Right (file, handle)
                )
                ( \e -> do
                    if isDoesNotExistError e
                      then putStrLn $ "error: file not found: " ++ path
                      else putStrLn $ "error loading file: " ++ displayException e
                    return $ Left e
                )

          case result of
            Right (file, handle) -> do
              let (newCtx, evalResult, newStack) = evaluate ctx file stack
              lift $ put newStack
              liftIO $ putStrLn $ Maybe.fromMaybe "ok" evalResult
              liftIO $ hClose handle
              local (const newCtx) replWithStack
            Left _ ->
              replWithStack
