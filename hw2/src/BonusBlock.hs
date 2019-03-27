{-# LANGUAGE ExistentialQuantification #-}

module BonusBlock
  ( Cont(..)
  , ForkTag(..)
  , ReadyProcess(..)
  , Syscall(..)
  , Waiting
  , example
  , exit
  , fork
  , forkExample
  , kernelIO
  , kernelPlayground
  , longForkExample
  , readLine
  , writeLine
  , yield
  , yieldExample
  ) where

import Control.Monad.State (State, StateT, evalStateT, get, lift, put, runState)
import Data.Dequeue (BankersDequeue, fromList, popFront, pushBack, pushFront)
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

data KernelState = KernelState
  { readyProcesses :: BankersDequeue ReadyProcess
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

pushBackProcess :: ReadyProcess -> State KernelState ()
pushBackProcess process = do
  curKernelState@KernelState {readyProcesses = curRunning} <- get
  let newRunning = pushBack curRunning process
  put curKernelState {readyProcesses = newRunning}

pushFrontProcess :: ReadyProcess -> State KernelState ()
pushFrontProcess process = do
  curKernelState@KernelState {readyProcesses = curRunning} <- get
  let newRunning = pushFront curRunning process
  put curKernelState {readyProcesses = newRunning}

kernelPlaygroundImpl :: State KernelState ()
kernelPlaygroundImpl = do
  curKernel@KernelState {readyProcesses = curReady, stdin = curStdin, stdout = curStdout} <- get
  case popFront curReady of
    Nothing -> return ()
    Just (ReadyProcess {computationToNextSyscall = curCompuation, processState = curState}, q) -> do
      case curCompuation curState of
        ReadSyscall onRead -> do
          let (readed, rest) = span (/= '\n') curStdin
          let newStdin =
                case rest of
                  []         -> []
                  (_:others) -> others
          put curKernel {stdin = newStdin, readyProcesses = q}
          pushFrontProcess $ ReadyProcess onRead readed
        WriteSyscall s onWrite ->
          put curKernel {stdout = curStdout ++ s ++ "\n", readyProcesses = pushFront q (ReadyProcess onWrite ())}
        ExitSyscall _ -> put curKernel {readyProcesses = q}
        ForkSyscall onFork -> do
          put curKernel {readyProcesses = q}
          pushFrontProcess $ ReadyProcess onFork Parent
          pushFrontProcess $ ReadyProcess onFork Child
        YieldSyscall onReturn -> do
          put curKernel {readyProcesses = q}
          pushBackProcess $ ReadyProcess onReturn ()
      kernelPlaygroundImpl

kernelPlayground :: Cont Syscall Void -> String -> String
kernelPlayground process input =
  let initProc = ReadyProcess (runCont process) lastCont
      initState = KernelState {readyProcesses = fromList [initProc], stdin = input, stdout = ""}
      (_, finalKernel) = runState kernelPlaygroundImpl initState
   in stdout finalKernel

lastCont :: Void -> Syscall
lastCont _ = ExitSyscall lastCont

pushBackState :: Monad m => a -> StateT (BankersDequeue a) m ()
pushBackState x = do
  curQueue <- get
  let newQueue = pushBack curQueue x
  put newQueue

pushFrontState :: Monad m => a -> StateT (BankersDequeue a) m ()
pushFrontState x = do
  curQueue <- get
  let newQueue = pushFront curQueue x
  put newQueue

kernelIOImpl :: StateT (BankersDequeue Syscall) IO ()
kernelIOImpl = do
  queue <- get
  case popFront queue of
    Nothing -> return ()
    Just (syscall, poppedQueue) -> do
      put poppedQueue
      case syscall of
        ReadSyscall onRead -> do
          s <- lift getLine
          pushFrontState (onRead s)
          kernelIOImpl
        WriteSyscall s onWrite -> do
          lift $ putStrLn s
          pushFrontState (onWrite ())
          kernelIOImpl
        ExitSyscall _ -> kernelIOImpl
        ForkSyscall onFork -> do
          pushFrontState (onFork Parent)
          pushFrontState (onFork Child)
          kernelIOImpl
        YieldSyscall onReturn -> do
          pushBackState (onReturn ())
          kernelIOImpl

kernelIO :: Cont Syscall Void -> IO ()
kernelIO process = evalStateT kernelIOImpl (fromList [runCont process (\_ -> ExitSyscall lastCont)])

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
      let str = "Hello, " ++ s ++ ", from parent process"
      writeLine str
      exit

longForkExample :: Cont Syscall Void
longForkExample = do
  pid <- fork
  case pid of
    Child -> do
      writeLine "Hello from child process"
      writeLine "Child process continues running"
      exit
    Parent -> do
      s <- readLine
      let str = "Hello, " ++ s ++ ", from parent process"
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
