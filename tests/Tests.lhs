\begin{code}
{-# language LambdaCase #-}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
{-# language DataKinds, TypeOperators, TypeApplications #-}
{-# language RankNTypes #-}
import Control.Monad.Freer as F
import Control.Monad.Freer.Proc as FP hiding (send)
import Control.Monad.Freer.Handler as FH
import Data.HVect
import Control.Monad (void)
import qualified Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Node as Node
import Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend, newLocalNode)
\end{code}
Below is required for the test
\begin{code}
import qualified Control.Monad.Freer.Reader as FR
import qualified Control.Monad.Freer.State as FS
import System.Exit hiding (ExitCode(ExitSuccess))

main = exitSuccess
\end{code}
runList testcode
\begin{code}
test1 :: Eff '[FS.State Int] Int
test1 =  FS.get

test2 :: Eff '[FR.Reader Int] Int
test2 = FR.ask

test :: Eff '[FR.Reader Int, FS.State Int] Int
test = do n <- FS.get @Int
          m <- FR.ask
          FS.put (n + m)
          FS.put (n + m + 1)
          FS.put (n + m + 2)
          FS.get @Int

testRun :: Int
testRun = run (FS.evalState 3 (FR.runReader 5 test))

test1Run3 :: Int
test1Run3 = run (runList ((Handler (FS.evalState 3)) :&: HNil) test1)

test2Run3 :: Int
test2Run3 = run (runList (Handler (FR.runReader 5) :&: HNil) test2)

testRun3 :: Int
testRun3 = run (runList (Handler (FR.runReader 5) :&: Handler (FS.evalState 3) :&: HNil) test)
\end{code}

The console code is a slightly more eleborate test. The code is taken from the freer-simple documentation.
\begin{code}
data Console r where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()

putStrLn' :: Member Console effs => String -> Eff effs ()
putStrLn' = send . PutStrLn

getLine' :: Member Console effs => Eff effs String
getLine' = send GetLine

exitSuccess' :: Member Console effs => Eff effs ()
exitSuccess' = send ExitSuccess

runConsole :: Eff '[Console, IO] a -> IO a
runConsole = runM . runConsole' id

runConsole' :: (LastMember m effs) => (forall a. IO a -> m a) -> Eff (Console ': effs) a -> Eff effs a
runConsole' liftIO = interpretM (\case
  PutStrLn msg -> liftIO $ putStrLn msg
  GetLine -> liftIO $ getLine
  ExitSuccess -> liftIO $ exitSuccess)
\end{code}

\begin{code}
testIO :: Eff '[Console, IO] ()
testIO = do putStrLn' "Hello, World"
            exitSuccess'

testIORun :: IO ()
testIORun = runConsole testIO
\end{code}

\begin{code}
testProcIO :: IO ()
testProcIO = do
    backend <- initializeBackend "127.0.0.1" "8230" Node.initRemoteTable
    node <- newLocalNode backend
    Node.runProcess node (void testProc)

testProcIO2 :: IO ()
testProcIO2 = do
    backend <- initializeBackend "127.0.0.1" "8231" Node.initRemoteTable
    node <- newLocalNode backend
    Node.runProcess node (void testProc)

testProc :: DP.Process ()
testProc = runProcess handlers prog
  where
    handlers = (Handler (FR.runReader "Hello") :&: Handler (FR.runReader (5 :: Int)) :&: HNil)
    prog = call f

testProc2 :: DP.Process FP.PID
testProc2 = runM (runProc handlers prog)
  where
    handlers = (Handler (FR.runReader "Hello") :&: Handler (FR.runReader (5 :: Int)) :&: HNil)
    prog = spawn j
\end{code}

\begin{code}
hh :: Eff '[FR.Reader Int] Int
hh = FR.ask

gh :: Eff '[FR.Reader String] String
gh = FR.ask

\end{code}

\begin{code}
f :: Eff [FR.Reader String, FR.Reader Int, FP.Proc '[]] ()
f = do s <- FR.ask @String
       i <- FR.ask @Int
       liftIO $ putStrLn (s ++ show i)
\end{code}
\begin{code}
g :: Eff '[FR.Reader Int, FP.Proc '[FR.Reader String]] ()
g = do s <- call gh
       i <- FR.ask @Int
       liftIO $ putStrLn (s ++ show i)

h :: Eff '[FR.Reader String, FP.Proc '[FR.Reader Int]] ()
h = do i <- call hh
       s <- FR.ask @String
       liftIO $ putStrLn (s ++ show i)
\end{code}

its type should still be a valid instance it just isn't that useful.
The code, however, shouldn't work

i :: Eff [Proc '[FR.Reader String], Proc '[FR.Reader Int]] ()
i = do i <- call hh
       s <- call gh
       sendM $ liftIO $ putStrLn (s ++ show i)

\begin{code}
j :: Eff '[FP.Proc '[FR.Reader String, FR.Reader Int]] ()
j = do i <- call hh
       s <- call gh
       liftIO $ putStrLn (s ++ show i)
\end{code}
