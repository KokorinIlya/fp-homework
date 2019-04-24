{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Interpreter
  ( parseAndProcessScript
  ) where

import Control.Exception (IOException, catch)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, execState, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Map.Strict as Map
import Parser (programParser)
import ProgramStructure (Assignment (..), Command (..), DoubleQuotesInner (..), ElseIf (..),
                         Identifier (..), If (..), ImplicitQuotesInner (..), ShellCommand (..),
                         Variable (..), While (..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.IO (hGetContents)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Text.Megaparsec (runParser)
import Text.Read (readMaybe)

data Environment = Environment
  { variables       :: IORef (Map.Map Identifier String)
  , scriptArguments :: Map.Map Int String
  , inlineCallDepth :: Int
  , currentOutput   :: IORef String
  }

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zipWithIndexImpl 0
  where
    zipWithIndexImpl :: Int -> [a] -> [(Int, a)]
    zipWithIndexImpl _ []     = []
    zipWithIndexImpl n (x:xs) = (n, x) : zipWithIndexImpl (n + 1) xs

data ReturnStatus
  = ExitCode Int
  | JustReturn Int
  deriving (Show)

processDoubleQuotes :: DoubleQuotesInner -> ReaderT Environment IO String
processDoubleQuotes DoubleQuotesEnd = return ""
processDoubleQuotes (DoubleQuotesSimpleString s r) = do
  remainder <- processDoubleQuotes r
  return $ s <> remainder
processDoubleQuotes (DoubleQuotesVariableRef (IdentifiedVariable identifier) r) = do
  curEnvironment <- ask
  curVariableMap <- lift $ readIORef (variables curEnvironment)
  let variableValue = Map.findWithDefault "" identifier curVariableMap
  remainder <- processDoubleQuotes r
  return $ variableValue <> remainder
processDoubleQuotes (DoubleQuotesVariableRef (ScriptArgument argNumber) r) = do
  curEnvironment <- ask
  let argumentValue = Map.findWithDefault "" argNumber (scriptArguments curEnvironment)
  remainder <- processDoubleQuotes r
  return $ argumentValue <> remainder
processDoubleQuotes (DoubleQuotesVariableRef (InlineCall commands) r) = do
  inlineCallResult <- processInlineCall commands
  remainder <- processDoubleQuotes r
  return $ inlineCallResult <> remainder

processImplicitQuotes :: ImplicitQuotesInner -> ReaderT Environment IO String
processImplicitQuotes ImplicitQuotesEnd = return ""
processImplicitQuotes (ImplicitQuotesSimpleString s r) = do
  remainder <- processImplicitQuotes r
  return $ s <> remainder
processImplicitQuotes (ImplicitQuotesVariableRef (IdentifiedVariable identifier) r) = do
  curEnvironment <- ask
  curVariableMap <- lift $ readIORef (variables curEnvironment)
  let variableValue = Map.findWithDefault "" identifier curVariableMap
  remainder <- processImplicitQuotes r
  return $ variableValue <> remainder
processImplicitQuotes (ImplicitQuotesVariableRef (ScriptArgument argNumber) r) = do
  curEnvironment <- ask
  let argumentValue = Map.findWithDefault "" argNumber (scriptArguments curEnvironment)
  remainder <- processImplicitQuotes r
  return $ argumentValue <> remainder
processImplicitQuotes (ImplicitQuotesDoubleQuotes quotes r) = do
  curString <- processDoubleQuotes quotes
  remainder <- processImplicitQuotes r
  return $ curString <> remainder
processImplicitQuotes (ImplicitQuotesVariableRef (InlineCall commands) r) = do
  inlineCallResult <- processInlineCall commands
  remainder <- processImplicitQuotes r
  return $ inlineCallResult <> remainder

makeCommand :: NonEmpty ImplicitQuotesInner -> ReaderT Environment IO (NonEmpty String)
makeCommand (argument :| []) = do
  mappedArgument <- processImplicitQuotes argument
  return $ mappedArgument :| []
makeCommand (argument :| (secondArgument:otherArguments)) = do
  mappedArgument <- processImplicitQuotes argument
  secondMappedArgument :| otherMappedArguments <- makeCommand (secondArgument :| otherArguments)
  return $ mappedArgument :| (secondMappedArgument : otherMappedArguments)

processRead :: [String] -> String -> Map.Map Identifier String -> Map.Map Identifier String
processRead variableNames readedLine =
  let variableIdentifiers = namesToIdentifiers variableNames
      stringParts = words readedLine
      varsWithValues = zipVarsWithValues variableIdentifiers stringParts
   in execState (addValuesToMap varsWithValues)
  where
    namesToIdentifiers :: [String] -> [Identifier]
    namesToIdentifiers [] = []
    namesToIdentifiers ("":names) = namesToIdentifiers names
    namesToIdentifiers ((char:chars):names) = Identifier (char :| chars) : namesToIdentifiers names
    zipVarsWithValues :: [Identifier] -> [String] -> [(Identifier, String)]
    zipVarsWithValues [] _              = []
    zipVarsWithValues [v] strs          = [(v, unwords strs)]
    zipVarsWithValues (v:vs) []         = (v, "") : zipVarsWithValues vs []
    zipVarsWithValues (v:vs) (str:strs) = (v, str) : zipVarsWithValues vs strs
    addValuesToMap :: [(Identifier, String)] -> State (Map.Map Identifier String) ()
    addValuesToMap [] = return ()
    addValuesToMap ((identifier, value):others) = do
      currentMap <- get
      put $ Map.insert identifier value currentMap
      addValuesToMap others

failWithMessage :: String -> a -> IOException -> IO a
failWithMessage s result _ = do
  putStrLn s
  return result

failWithReturnCode :: String -> Int -> IOException -> IO ReturnStatus
failWithReturnCode s n = failWithMessage s (JustReturn n)

processEcho :: [String] -> ReaderT Environment IO ReturnStatus
processEcho ("-n":args) =
  lift $ (putStr (unwords args) >> return (JustReturn 0)) `catch` failWithReturnCode "Error writing to console" 1
processEcho args =
  lift $ (putStrLn (unwords args) >> return (JustReturn 0)) `catch` failWithReturnCode "Error writing to console" 1

processPwd :: ReaderT Environment IO ReturnStatus
processPwd = lift $ (processPwdImpl >> return (JustReturn 0)) `catch` failWithReturnCode "Error in pwd" 1
  where
    processPwdImpl :: IO ()
    processPwdImpl = do
      currentDirectory <- getCurrentDirectory
      putStrLn currentDirectory

processExit :: [String] -> IO ReturnStatus
processExit [arg] =
  let maybeExitCode = readMaybe @Int arg
   in case maybeExitCode of
        Nothing -> do
          putStrLn $ "Couldn't convert " <> show arg <> " to exit code"
          return $ JustReturn 1
        Just exitCode -> return $ ExitCode exitCode
processExit args = do
  putStrLn $ "Wrong number of arguments for exit: received " <> show args
  return $ JustReturn 1

processCd :: [String] -> ReaderT Environment IO ReturnStatus
processCd [arg] = lift $ (setCurrentDirectory arg >> return (JustReturn 0)) `catch` failWithReturnCode "Error in cd" 1
processCd args = do
  lift $ putStrLn $ "Wrong number of arguments in cd: " <> show args
  return $ JustReturn 1

safeRunIO :: IO a -> (a -> b) -> String -> b -> IO b
safeRunIO action f msg def = (f <$> action) `catch` failWithMessage msg def

runExternalProcess :: String -> [String] -> ReaderT Environment IO ReturnStatus
runExternalProcess pName pArgs = do
  curEnv <- ask
  maybeReturnStatus <- lift $ runMaybeT $ runReaderT (runExternalProcessImpl pName pArgs) curEnv
  case maybeReturnStatus of
    Nothing           -> return $ JustReturn 1
    Just returnStatus -> return returnStatus
  where
    runExternalProcessImpl :: String -> [String] -> ReaderT Environment (MaybeT IO) ReturnStatus
    runExternalProcessImpl processName processArgs = do
      let processInfo = (proc processName processArgs) {cwd = Nothing, std_out = CreatePipe} -- TODO: add Inherit
      (_, Just stdoutHandle, _, procHandle) <-
        lift $ MaybeT $ safeRunIO (createProcess processInfo) Just "Error while creating process" Nothing
      code <- lift $ MaybeT $ safeRunIO (waitForProcess procHandle) Just "Error while waiting for process" Nothing
      let returnStatus =
            case code of
              ExitSuccess   -> JustReturn 0
              ExitFailure c -> JustReturn c
      output <- lift $ MaybeT $ safeRunIO (hGetContents stdoutHandle) Just "Error while getting process output" Nothing
      processOutput output
      return returnStatus
    processOutput :: String -> ReaderT Environment (MaybeT IO) ()
    processOutput output = do
      curEnvironment <- ask
      if inlineCallDepth curEnvironment > 0
        then do
          let curOutoutRef = currentOutput curEnvironment
          curOutputValue <- lift $ lift $ readIORef curOutoutRef
          lift $ lift $ writeIORef curOutoutRef (curOutputValue <> output)
        else lift $ MaybeT $ safeRunIO (putStrLn output) Just "Error while writing output" Nothing

processWhile :: Int -> While -> ReaderT Environment IO ReturnStatus
processWhile n while@While {whileConditions = curWhileConditions, whileActions = curWhileActions} = do
  conditionsReturnStatus <- processScript $ toList curWhileConditions
  case conditionsReturnStatus of
    exitCode@(ExitCode _) -> return exitCode
    JustReturn code
      | code == 0 -> do
        commandsReturnStatus <- processScript curWhileActions
        case commandsReturnStatus of
          exitCode@(ExitCode _) -> return exitCode
          JustReturn returnCode -> processWhile returnCode while
      | otherwise -> return $ JustReturn n

processIf :: If -> ReaderT Environment IO ReturnStatus
processIf If { ifConditions = curIfConditions
             , ifActions = curActions
             , elseIfs = curElseIfs
             , maybeElse = maybeElseActions
             } = do
  conditionsReturnStatus <- processScript $ toList curIfConditions
  case conditionsReturnStatus of
    exitCode@(ExitCode _) -> return exitCode
    JustReturn code
      | code == 0 -> processScript curActions
      | otherwise -> do
        maybeElseIfResult <- processElseIfs curElseIfs
        case maybeElseIfResult of
          Nothing ->
            case maybeElseActions of
              Nothing          -> return $ JustReturn 0
              Just elseActions -> processScript elseActions
          Just elseIfResult -> return elseIfResult
  where
    processElseIfs :: [ElseIf] -> ReaderT Environment IO (Maybe ReturnStatus)
    processElseIfs [] = return Nothing
    processElseIfs (ElseIf {elseIfConditions = curElseIfConditions, elseIfActions = curElseIfActions}:others) = do
      conditionsReturnStatus <- processScript $ toList curElseIfConditions
      case conditionsReturnStatus of
        exitCode@(ExitCode _) -> return $ Just exitCode
        JustReturn code
          | code == 0 -> Just <$> processScript curElseIfActions
          | otherwise -> processElseIfs others

processCommand :: Command -> ReaderT Environment IO ReturnStatus
processCommand (AssignmentCommand (Assignment left right)) = do
  curEnvironment <- ask
  curVariables <- lift $ readIORef $ variables curEnvironment
  rightString <- processImplicitQuotes right
  --lift $ putStrLn $ "Variable: " <> show left <> ", value: " <> rightString
  lift $ writeIORef (variables curEnvironment) (Map.insert left rightString curVariables)
  return $ JustReturn 0
processCommand (CallCommand (ShellCommand commandParts)) = do
  curEnvironment <- ask
  curVariables <- lift $ readIORef $ variables curEnvironment
  command :| commandArguments <- makeCommand commandParts
  case command of
    "read" -> do
      maybeReadedLine <- lift $ safeRunIO getLine Just "Error while reading line" Nothing
      case maybeReadedLine of
        Just readedLine -> do
          lift $ writeIORef (variables curEnvironment) (processRead commandArguments readedLine curVariables)
          return $ JustReturn 0
        Nothing -> return $ JustReturn 1
    "echo" -> processEcho commandArguments
    "pwd" -> processPwd
    "exit" -> lift $ processExit commandArguments
    "cd" -> processCd commandArguments
    _ -> runExternalProcess command commandArguments
processCommand (WhileCommand while) = processWhile 0 while
processCommand (IfCommand ifCommand) = processIf ifCommand

processInlineCall :: [Command] -> ReaderT Environment IO String
processInlineCall commands = do
  curEnvironment <- ask
  let curInlineCallDepth = inlineCallDepth curEnvironment
  variablesMap <- lift $ readIORef (variables curEnvironment)
  newMapRef <- lift $ newIORef variablesMap
  newOutputRef <- lift $ newIORef ""
  let newEnvironment = Environment newMapRef (scriptArguments curEnvironment) (curInlineCallDepth + 1) newOutputRef
  _ <- lift $ runReaderT (processScript commands) newEnvironment
  lift $ readIORef newOutputRef

processScript :: [Command] -> ReaderT Environment IO ReturnStatus
processScript [] = return $ ExitCode 0
processScript [curCommand] = processCommand curCommand
processScript (curCommand:nextCommand:otherCommands) = do
  curReturnStatus <- processCommand curCommand
  case curReturnStatus of
    exitCode@(ExitCode _) -> return exitCode
    JustReturn _          -> processScript (nextCommand : otherCommands)

parseAndProcessScript :: String -> [String] -> IO ()
parseAndProcessScript script args =
  case runParser programParser "" script of
    Left parsingError -> putStrLn $ "Error while parsing script: " <> show parsingError
    Right parserResult -> do
      emptyVariablesMap <- newIORef Map.empty
      startOutput <- newIORef ""
      let scriptArgsMap = Map.fromList $ zipWithIndex args
      let startCtx = Environment emptyVariablesMap scriptArgsMap 0 startOutput
      returnStatus <- runReaderT (processScript parserResult) startCtx
      let exitCode =
            case returnStatus of
              ExitCode code   -> code
              JustReturn code -> code
      putStrLn $ "Script execution finished with exit code " <> show exitCode
