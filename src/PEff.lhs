\begin{code}
{-# language GADTs #-}
{-# language DataKinds, TypeOperators #-}
{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
import qualified Control.Monad.Freer as F
import qualified Control.Monad.Freer.Internal as FI
import qualified Control.Monad.Freer.Reader as FR
import Data.OpenUnion ((:++:))
import Control.Distributed.Process (ProcessId, liftIO, Process, spawnLocal)
import Data.Proxy
\end{code}
\begin{code}
main = undefined
\end{code}

\begin{code}
type PEff r fr a = F.Eff (Proc ': r) a
\end{code}


Gebruik maken van lists voor non-determinism?
Op deze manier kunnen we stappelen en dan een andere interpreter gebruiken voor de Process events, iederen van wie weer binnen de rest moet draaien.

\begin{code}
runPEff :: (F.LastMember Process r) => PEff r rf a -> F.Eff r a
runPEff (eff) = F.interpretM f eff
  where
   f :: (Proc a -> Process a)
   f (Spawn p s) = (spawnLocal _)
\end{code}

\begin{code}

type PID = ProcessId

data Proc a where
  Spawn :: (F.Members r' r) => Proxy r -> PEff r' fr' () -> Proc PID
  Call  :: (F.Members r' r) => Proxy r -> PEff r' fr' a -> Proc a
  Send :: a -> Proc ()
  Say  :: String -> Proc ()
  Expect :: Proc a
\end{code}

\begin{code}
\end{code}

\begin{code}

spawn :: forall r' rf' r rf. (F.Members r' r) => PEff r' rf' () -> PEff r rf PID
spawn p = send (Spawn (Proxy :: Proxy r) p)

send :: (F.Member eff (Proc ': effs)) => eff a -> PEff effs rf a
send t = (FI.E (FI.inj t) (FI.tsingleton FI.Val))



\end{code}

lift :: F.Eff r a -> PEff r rf a
lift = upgrade . F.raise

upgrade :: F.Eff (Proc ': r) a -> PEff r rf a
upgrade = PEff

Try to factor out the runPEff, maybe using package/existential types
Use call for runPEff?
raise :: (F.LastMember Process r) => PEff r fr a -> PEff (e ': r) fr a
raise p = PEff (F.raise (F.raise (runPEff p)))
