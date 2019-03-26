{-# LANGUAGE ExistentialQuantification #-}

module BonusBlock
  ( Cont(..)
  , PausedProcess(..)
  , Waiting
  , ReadyProcess(..)
  , kernel
  , readLine
  , example
  , writeLine
  , exit
  ) where

import Control.Monad.State (State, get, put, runState)
import Data.Void (Void)
import Prelude hiding (read)

newtype Cont r a = Cont
  { runCont :: (a -> r) -> r
  }

instance Functor (Cont r) where
  fmap f (Cont continuation) = Cont $ \onComplete -> continuation (onComplete . f)

instance Applicative (Cont r) where
  pure value = Cont $ \onComplete -> onComplete value
  Cont contF <*> Cont contA = Cont $ \onComplete -> contF (\f -> contA (onComplete . f))

instance Monad (Cont r) where
  (Cont cont) >>= f = Cont $ \onComplete -> cont (\aValue -> runCont (f aValue) onComplete)

data Queue a = Queue
  { headList :: [a]
  , tailList :: [a]
  }

empty :: Queue a
empty = Queue {headList = [], tailList = []}

singleton :: a -> Queue a
singleton x = Queue {tailList = [x], headList = []}

pop :: Queue a -> Maybe (a, Queue a)
pop q@Queue {tailList = first:others} = Just (first, q {tailList = others})
pop Queue {headList = curHeadList, tailList = []} =
  case reverse curHeadList of
    first:others -> Just (first, Queue {headList = [], tailList = others})
    []           -> Nothing

push :: Queue a -> a -> Queue a
push q@Queue {headList = curHeadList} x = q {headList = x : curHeadList}

data KernelState = KernelState
  { readyProcesses :: Queue ReadyProcess
  , stdin          :: String
  , stdout         :: String
  }

type Waiting r = r -> PausedProcess

data PausedProcess
  = ReadPaused (Waiting String)
  | WritePaused String
                (Waiting ())
  | ExitPaused (Waiting Void)

data ReadyProcess = forall r. ReadyProcess
  { computationToNextSyscall :: Waiting r
  , processState             :: r
  }

readLineAction :: Waiting String -> State KernelState ()
readLineAction onRead = do
  curKernelState@KernelState {stdin = curStdin} <- get
  let (readed, rest) = span (/= '\n') curStdin
  let newStdin =
        case rest of
          []         -> []
          (_:others) -> others
  put curKernelState {stdin = newStdin}
  addProcess $ ReadyProcess onRead readed

writeLineAction :: String -> Waiting () -> State KernelState ()
writeLineAction s onWrite = do
  curKernel@KernelState {stdout = curStdout} <- get
  put curKernel {stdout = curStdout ++ s ++ "\n"}
  addProcess $ ReadyProcess onWrite ()

exitAction :: Waiting Void -> State KernelState ()
exitAction _ = do
  curKernelState <- get
  put curKernelState {readyProcesses = empty}

addProcess :: ReadyProcess -> State KernelState ()
addProcess process = do
  curKernelState@KernelState {readyProcesses = curRunning} <- get
  let newRunning = push curRunning process
  put curKernelState {readyProcesses = newRunning}

kernelImpl :: State KernelState ()
kernelImpl = do
  curKernel@KernelState {readyProcesses = curReady} <- get
  case pop curReady of
    Nothing -> return ()
    Just (ReadyProcess {computationToNextSyscall = curCompuation, processState = curState}, q) -> do
      put curKernel {readyProcesses = q}
      case curCompuation curState of
        ReadPaused onRead     -> readLineAction onRead
        WritePaused s onWrite -> writeLineAction s onWrite
        ExitPaused onExit     -> exitAction onExit
      kernelImpl

kernel :: Cont PausedProcess Void -> String -> String
kernel process input =
  let f :: Void -> PausedProcess
      f _ = ExitPaused f
      initProc = ReadyProcess (runCont process) f
      initState = KernelState {readyProcesses = singleton initProc, stdin = input, stdout = ""}
      (_, finalKernel) = runState kernelImpl initState
   in stdout finalKernel

readLine :: Cont PausedProcess String
readLine = Cont $ \c -> ReadPaused c

writeLine :: String -> Cont PausedProcess ()
writeLine s = Cont $ \c -> WritePaused s c

exit :: Cont PausedProcess Void
exit = Cont $ \c -> ExitPaused c

example :: Cont PausedProcess Void
example = do
  s <- readLine
  let str = "Hello, " ++ s ++ "!"
  writeLine str
  s' <- readLine
  let str' = "Also, hello, " ++ s' ++ "!"
  writeLine str'
  exit
