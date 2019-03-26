{-# LANGUAGE ExistentialQuantification #-}

module BonusBlock
  ( Cont(..)
  , Syscall(..)
  , Waiting
  , ReadyProcess(..)
  , kernel
  , readLine
  , example
  , writeLine
  , exit
  , fork
  , forkExample
  , yield
  , yieldExample
  ) where

import Control.Monad.State (State, get, put, runState)
import Data.Void (Void)

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

type Waiting r = r -> Syscall

data ForkTag
  = Child
  | Parent

data Syscall
  = ReadSyscall (Waiting String)
  | WriteSyscall String
                 (Waiting ())
  | ExitSyscall (Waiting Void)
  | ForkSyscall (Waiting ForkTag)
  | YieldSyscall (Waiting ())

data ReadyProcess = forall r. ReadyProcess
  { computationToNextSyscall :: Waiting r
  , processState             :: r
  }

yieldAction :: Waiting () -> State KernelState ()
yieldAction onReturn = addProcess $ ReadyProcess onReturn ()

forkAction :: Waiting ForkTag -> State KernelState ()
forkAction onFork = do
  addProcess $ ReadyProcess onFork Child
  addProcess $ ReadyProcess onFork Parent

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
exitAction _ = return ()

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
        ReadSyscall onRead     -> readLineAction onRead
        WriteSyscall s onWrite -> writeLineAction s onWrite
        ExitSyscall onExit     -> exitAction onExit
        ForkSyscall onFork     -> forkAction onFork
        YieldSyscall onReturn  -> yieldAction onReturn
      kernelImpl

kernel :: Cont Syscall Void -> String -> String
kernel process input =
  let f :: Void -> Syscall
      f _ = ExitSyscall f
      initProc = ReadyProcess (runCont process) f
      initState = KernelState {readyProcesses = singleton initProc, stdin = input, stdout = ""}
      (_, finalKernel) = runState kernelImpl initState
   in stdout finalKernel

readLine :: Cont Syscall String
readLine = Cont $ \c -> ReadSyscall c

writeLine :: String -> Cont Syscall ()
writeLine s = Cont $ \c -> WriteSyscall s c

exit :: Cont Syscall Void
exit = Cont $ \c -> ExitSyscall c

fork :: Cont Syscall ForkTag
fork = Cont $ \c -> ForkSyscall c

yield :: Cont Syscall ()
yield = Cont $ \c -> YieldSyscall c

example :: Cont Syscall Void
example = do
  s <- readLine
  let str = "Hello, " ++ s ++ "!"
  writeLine str
  s' <- readLine
  let str' = "Also, hello, " ++ s' ++ "!"
  writeLine str'
  exit

forkExample :: Cont Syscall Void
forkExample = do
  pid <- fork
  case pid of
    Child -> do
      writeLine "Hello from child process"
      exit
    Parent -> do
      s <- readLine
      let str = "Hello, " ++ s ++ " from parent process"
      writeLine str
      exit

yieldExample :: Cont Syscall Void
yieldExample = do
  pid <- fork
  case pid of
    Child -> do
      writeLine "Hello from child process, child process is yielding"
      yield
      writeLine "Child process is back!"
      exit
    Parent -> do
      s <- readLine
      let str = "Hello, " ++ s ++ ", from parent process"
      writeLine str
      exit
