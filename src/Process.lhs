\begin{code}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
module Process (
  Process,
  spawn,
  call,
  send,
  expect
  ) where

import Control.Monad.Freer hiding (send)
import qualified Control.Monad.Freer as F (send)
import qualified Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Node as DPN

type PID = DP.ProcessId

data ProcessCreate effs r where
  Spawn :: Eff effs () -> ProcessCreate effs PID
  Call :: Eff effs r -> ProcessCreate effs r

data ProcessComm r where
  Send :: PID -> a -> ProcessComm ()
  Expect :: ProcessComm a
  Say :: String -> ProcessComm ()

spawn :: (Member Process effs, Member Process effs') => Eff effs' () -> Eff effs PID
spawn = F.send . Spawn

call :: (Member Process effs, Member Process effs') => Eff effs' r -> Eff effs r
call = F.send . Call

send :: (Member Process effs) => PID -> a -> Eff effs ()
send pid = F.send . Send pid

expect :: (Member Process effs) => Eff effs a
expect = F.send Expect

say :: (Member Process effs) => String -> Eff effs ()
say = F.send . Say
\end{code}

runProcess hoeft misschien niet zo'n precies type the hebben.

Beter is waarschijnlijk iets zoals:
runReader :: forall r effs a. r -> Eff (Reader r ': effs) a -> Eff effs a

\begin{code}
  runProcess :: Eff '[ProcessCreate effs] ': effs () -> Eff effs ()
  runProcess = intepret f
    where f :: ProcessCreate effs r -> Eff effs ()
          f (Spawn p) = forkProcess
\end{code}
