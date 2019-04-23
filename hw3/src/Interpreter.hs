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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Parser (programParser)
import ProgramStructure (Assignment (..), Command (..), DoubleQuotesInner (..), ElseIf (..),
                         Identifier (..), If (..), ImplicitQuotesInner (..), ShellCommand (..),
                         SingleQuotes (..), Variable (..), While (..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Text.Megaparsec (runParser)
import Text.Read (readMaybe)

data Environment = Environment
  { _variables       :: IORef (Map.Map Identifier String)
  , _scriptArguments :: Map.Map Int String
  }

makeLenses ''Environment

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

processDoubleQuotes :: Map.Map Identifier String -> Map.Map Int String -> DoubleQuotesInner -> String
processDoubleQuotes _ _ DoubleQuotesEnd = ""
processDoubleQuotes vars args (DoubleQuotesSimpleString s r) = s <> processDoubleQuotes vars args r
processDoubleQuotes vars args (DoubleQuotesVariableRef (IdentifiedVariable identifier) r) =
  let variableValue = Map.findWithDefault "" identifier vars
   in variableValue <> processDoubleQuotes vars args r
processDoubleQuotes vars args (DoubleQuotesVariableRef (ScriptArgument argNumber) r) =
  let argumentValue = Map.findWithDefault "" argNumber args
   in argumentValue <> processDoubleQuotes vars args r

processImplicitQuotes :: Map.Map Identifier String -> Map.Map Int String -> ImplicitQuotesInner -> String
processImplicitQuotes _ _ ImplicitQuotesEnd = ""
processImplicitQuotes vars args (ImplicitQuotesVariableRef (IdentifiedVariable identifier) r) =
  let variableValue = Map.findWithDefault "" identifier vars
   in variableValue <> processImplicitQuotes vars args r
processImplicitQuotes vars args (ImplicitQuotesVariableRef (ScriptArgument argNumber) r) =
  let argumentValue = Map.findWithDefault "" argNumber args
   in argumentValue <> processImplicitQuotes vars args r
processImplicitQuotes vars args (ImplicitQuotesSimpleString s r) = s <> processImplicitQuotes vars args r
processImplicitQuotes vars args (ImplicitQuotesDoubleQuotes quotes r) =
  processDoubleQuotes vars args quotes <> processImplicitQuotes vars args r

makeCommand :: Map.Map Identifier String -> Map.Map Int String -> NonEmpty ImplicitQuotesInner -> NonEmpty String
makeCommand curVariables curScriptsArgs = fmap (processImplicitQuotes curVariables curScriptsArgs)

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

processCommand :: Command -> ReaderT Environment IO ReturnStatus
processCommand (AssignmentCommand (Assignment left right)) = do
  env <- ask
  curVariables <- lift $ readIORef $ env ^. variables
  let curScriptsArgs = env ^. scriptArguments
  let rightString = processImplicitQuotes curVariables curScriptsArgs right
  --lift $ putStrLn $ "Variable: " <> show left <> ", value: " <> rightString
  lift $ writeIORef (env ^. variables) (Map.insert left rightString curVariables)
  return $ JustReturn 0
processCommand (CallCommand (ShellCommand commandParts)) = do
  env <- ask
  curVariables <- lift $ readIORef $ env ^. variables
  let curScriptsArgs = env ^. scriptArguments
  let command :| commandArguments = makeCommand curVariables curScriptsArgs commandParts
  case command of
    "read" -> do
      maybeReadedLine <- lift $ (Just <$> getLine) `catch` failWithMessage "Error while reading line" Nothing
      case maybeReadedLine of
        Just readedLine -> do
          lift $ writeIORef (env ^. variables) (processRead commandArguments readedLine curVariables)
          return $ JustReturn 0
        Nothing -> return $ JustReturn 1
    "echo" -> processEcho commandArguments
    "pwd" -> processPwd
    "exit" -> lift $ processExit commandArguments
    "cd" -> processCd commandArguments
  {-lift $ putStrLn $ f x
  return $ JustReturn 0
  where
    f :: NonEmpty String -> String
    f (s :| [])       = s
    f (s :| (ss:sss)) = s <> "\n" <> f (ss :| sss)-}

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
      let scriptArgsMap = Map.fromList $ zipWithIndex args
      let startCtx = Environment emptyVariablesMap scriptArgsMap
      returnStatus <- runReaderT (processScript parserResult) startCtx
      let exitCode =
            case returnStatus of
              ExitCode code   -> code
              JustReturn code -> code
      putStrLn $ "Script execution finished with exit code " <> show exitCode
