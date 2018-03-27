To allow type level lists
\begin{code}
{-# language KindSignatures, DataKinds, TypeOperators #-}
\end{code}
\begin{code}
{-# language RankNTypes #-}
\end{code}
To allow the definition of effects
\begin{code}
{-# language GADTs #-}
\end{code}
\begin{code}
{-# language TypeFamilies, TypeFamilyDependencies #-}
\end{code}
Required for gets
\begin{code}
{-# language TypeApplications #-}
\end{code}

For the IO test
\begin{code}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
\end{code}

\begin{code}
module P (testRun) where
import Control.Monad.Freer
import qualified Control.Monad.Freer.Internal as FI
import qualified Control.Monad.Freer.Reader as FR
import qualified Control.Monad.Freer.State as FS
import Data.HVect as HV
import Data.OpenUnion ((:++:))
import Control.Distributed.Process (ProcessId, liftIO, Process, spawnLocal)
import Data.Proxy
import Unsafe.Coerce
import Debug.Trace
import Control.Monad.IO.Class
\end{code}

For the IO tests
\begin{code}
import System.Exit hiding (ExitCode(ExitSuccess))
\end{code}

\begin{code}
type PID = ProcessId
\end{code}

En het bevat de argument waar je mee wilt werken. Het enige dat het nog mist is de effect

\begin{code}
data RunList (r :: [* -> *])
data Proc r a where
  Spawn ::   RunList r' -> Eff r' () -> Proc r PID
  Call ::   RunList r' -> Eff r' () -> Proc r PID
  Send  :: a -> PID ->  Proc r a
  Expect :: Proc r a
\end{code}

\begin{code}

runPure :: HVect '[] -> Eff '[] a -> a
runPure HNil = run

type Handler b effs a = Eff (b ': effs) a -> Eff effs a

data HandlerM b effs a n = (LastMember n effs, Monad n) => HandlerM {runHandlerM :: Eff (b ': effs) a -> Eff effs a}


runHead :: HVect ((Handler b effs a) ': hf) -> Eff (b ': effs) a -> Eff effs a
runHead rl eff = r eff
  where r = findFirst rl

runHeadList :: HVect (HandlerList (b ': effs) a) -> Eff (b ': effs) a -> Eff effs a
runHeadList rl eff = r eff
  where r = findFirst rl

\end{code}
runList :: forall eff effs a . HVect (HandlerList (eff ': effs) a) -> Eff (eff ': effs) a -> Eff '[] a
runList (r :&: HNil) fect = let r' = (unsafeCoerce r) :: (Handler eff '[] a)
                                fect' = (unsafeCoerce fect) :: Eff '[eff] a
                            in r' fect'
runList (r :&: rs) fect =  let r' = (unsafeCoerce r) :: (Handler eff (eff' ': effs') a)
                               fect' = (unsafeCoerce rs) :: Eff (eff ': eff' ': effs') a
                               rs' = (unsafeCoerce rs) :: HVect (HandlerList (eff' ': effs') a)
                           in runList rs' (r' fect')
\begin{code}

runListAll :: forall effs a. HVect (HandlerList effs a) -> Eff effs a -> a
runListAll a b = run (runList a b)

runList :: forall effs a. HVect (HandlerList effs a) -> Eff effs a -> Eff '[] a
runList HNil fect = unsafeCoerce fect
runList (r :&: rs) fect = helper Proxy Proxy
  where helper :: forall eff' effs' a. Proxy eff' -> Proxy effs' -> Eff '[] a
        helper a b =
          let fect' = (unsafeCoerce fect) :: Eff (eff' ': effs') a
              r' = (unsafeCoerce r) :: Handler eff' effs' b
          in runList (unsafeCoerce rs) (r' fect')


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

\end{code}
testRun2 :: Int
testRun2 = run $ runHeadList rsl _
  where rsl = (FR.runReader 5) :&: sl
        sl = (FS.evalState 3) :&: Nil

\begin{code}
test1Run3 :: Int
test1Run3 = run (runList ((FS.evalState 3) :&: HNil) test1)

test2Run3 :: Int
test2Run3 = run (runList ((FR.runReader 5) :&: HNil) test2)

testRun3 :: Int
testRun3 = run (runList ((FR.runReader 5) :&: (FS.evalState 3) :&: HNil) test)

decompHList :: HVect (HandlerList (eff ': effs) a) -> (Handler eff effs a, HVect (HandlerList effs a))
decompHList eff = (findFirst eff, HV.tail eff)
\end{code}
\begin{code}
type family HandlerList effs a where
  HandlerList (eff ': effs) a = (Handler eff effs a) ': HandlerList effs a
  HandlerList '[] a =  '[]

type family HandlerListM effs a n where
  HandlerListM '[n] a n = '[n]
  HandlerListM (eff ': effs) a n = (HandlerM eff effs a n) ': HandlerListM effs a n

runListM :: forall effs a n. (LastMember n effs, Monad n) => HVect (HandlerListM effs a n) -> Eff effs a -> Eff '[n] a
runListM HNil fect = unsafeCoerce fect
runListM (r :&: rs) fect = helper Proxy Proxy Proxy
  where helper :: forall eff' effs' a n. (Monad n, LastMember n effs') => Proxy n -> Proxy eff' -> Proxy effs' -> Eff '[n] a
        helper a b c =
          let fect' = (unsafeCoerce fect) :: Eff (eff' ': effs') a
              r' = (unsafeCoerce r) :: HandlerM eff' effs' b n
          in runListM (unsafeCoerce rs) (runHandlerM r' fect')
\end{code}

Add a check that can test wheter HandlerList effs a =>

 Add decomposition operator which splits it into 
\begin{code}
\end{code}

HVectElim looks interesting just do not know how to apply it

Toch een data family nodig?

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
runConsole = runM . runConsole'

runConsole' :: (MonadIO m, LastMember m effs) => Eff (Console ': effs) a -> Eff effs a
runConsole' = interpretM (\case
  PutStrLn msg -> liftIO $ putStrLn msg
  GetLine -> liftIO $ getLine
  ExitSuccess -> liftIO $ exitSuccess)
\end{code}

\begin{code}
testIO :: Eff '[Console, IO] ()
testIO = do putStrLn' "Hello, World"
            exitSuccess'

testIORun = runConsole testIO

testIORun2 = runListM (runConsole' :&: HNil) testIO
\end{code}