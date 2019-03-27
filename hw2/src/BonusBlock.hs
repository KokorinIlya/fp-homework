{-# LANGUAGE ExistentialQuantification #-}

module BonusBlock
  ( Cont(..)
  , Syscall(..)
  , Waiting
  , ReadyProcess(..)
  , ForkTag(..)
  , kernelPlayground
  , readLine
  , example
  , writeLine
  , exit
  , fork
  , forkExample
  , yield
  , yieldExample
  , kernelIO
  ) where

import Control.Monad.State (StateT, evalStateT, lift, put, runState, get, State)
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

push :: a -> Queue a -> Queue a
push x q@Queue {headList = curHeadList} = q {headList = x : curHeadList}

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
  let newRunning = push process curRunning
  put curKernelState {readyProcesses = newRunning}

kernelPlaygroundImpl :: State KernelState ()
kernelPlaygroundImpl = do
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
      kernelPlaygroundImpl

kernelPlayground :: Cont Syscall Void -> String -> String
kernelPlayground process input =
  let initProc = ReadyProcess (runCont process) lastCont
      initState = KernelState {readyProcesses = singleton initProc, stdin = input, stdout = ""}
      (_, finalKernel) = runState kernelPlaygroundImpl initState
   in stdout finalKernel

lastCont :: Void -> Syscall
lastCont _ = ExitSyscall lastCont

pushState :: Monad m => a -> StateT (Queue a) m ()
pushState x = do
  curQueue <- get
  let newQueue = push x curQueue
  put newQueue


kernelIOImpl :: StateT (Queue Syscall) IO ()
kernelIOImpl = do
  queue <- get
  case pop queue of
    Nothing -> return ()
    Just (syscall, poppedQueue) -> do
      put poppedQueue
      case syscall of
        ReadSyscall onRead -> do
          s <- lift getLine
          pushState (onRead s)
          kernelIOImpl
        WriteSyscall s onWrite -> do
          lift $ putStrLn s
          pushState (onWrite ())
          kernelIOImpl
        ExitSyscall _ -> kernelIOImpl
        ForkSyscall onFork -> do
          pushState (onFork Child)
          pushState (onFork Parent)
          kernelIOImpl
        YieldSyscall onReturn -> do
          pushState (onReturn ())
          kernelIOImpl

kernelIO :: Cont Syscall Void -> IO ()
kernelIO process = evalStateT kernelIOImpl (singleton $ runCont process (\_ -> ExitSyscall lastCont))

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
