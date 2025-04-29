module Repl (repl) where

import Context
import Control.Exception (Exception (..), catch)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.State.Strict
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Eval (evaluate)
import File (fetchProgram)
import Parser (reservedKeywords)
import qualified Stack as S
import System.Console.Haskeline
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)

type ForthM a = ReaderT Context IO a

helpMsg :: String
helpMsg =
  "miniforth help\n\
  \:h or :help - show this help message\n\
  \:q or :quit - exit the interpreter\n\
  \:clear - clear the stack\n\
  \:words - show defined words\n\
  \:r - reload loaded file\n\
  \:l <path> or :load <path> - load a file\n"

commands :: [String]
commands =
  [ ":help",
    ":q",
    ":quit",
    ":clear",
    ":words",
    ":l",
    ":load"
  ]

forthCompletion :: CompletionFunc IO
forthCompletion = completeWord Nothing [] forthCompleter
  where
    forthCompleter :: String -> IO [Completion]
    forthCompleter str = do
      let matches = filter (List.isPrefixOf str) (reservedKeywords ++ commands)
      return $ map simpleCompletion matches

settings :: Settings IO
settings =
  Settings
    { complete = forthCompletion,
      historyFile = Just ".miniforth-history",
      autoAddHistory = True
    }

repl :: IO ()
repl = runReaderT replLoop Context.empty
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
      input <- liftIO $ runInputT settings $ getInputLine ">>> "

      case input of
        Nothing -> return ()
        Just cmd -> case parseCommand cmd of
          (":load", args) -> do
            ctx <- ask
            loadFile args ctx
          (":l", args) -> do
            ctx <- ask
            loadFile args ctx
          (":r", _) -> do
            ctx <- ask
            case loadedFile ctx of
              Nothing -> do
                liftIO $ putStrLn "no file loaded"
                replWithStack
              Just path -> do
                liftIO $ putStrLn $ "reloading file " ++ path
                loadFile [path] ctx
          (":h", _) -> do
            liftIO $ putStrLn helpMsg
          (":help", _) -> do
            liftIO $ putStrLn helpMsg

            replWithStack
          (":q", _) -> return ()
          (":quit", _) -> return ()
          (":clear", _) -> do
            ctx <- ask

            liftIO $ putStrLn "stack cleared"

            local (const $ Context.withStack S.empty ctx) replWithStack
          (":words", _) -> do
            ctx <- ask
            liftIO $ putStrLn $ "defined words: " ++ show (Map.keys $ Context.definedWords ctx)

            replWithStack
          (_, _) -> do
            ctx <- ask
            let (newCtx, result) = evaluate ctx cmd
            liftIO $ putStrLn $ Maybe.fromMaybe "ok" result
            local (const newCtx) replWithStack

    loadFile :: [String] -> Context -> ForthM ()
    loadFile args ctx = do
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
              let (newCtx, evalResult) = evaluate (withFile (Just path) ctx) file
              liftIO $ putStrLn $ Maybe.fromMaybe "ok" evalResult
              liftIO $ hClose handle
              local (const newCtx) replWithStack
            Left _ ->
              replWithStack
