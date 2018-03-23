\begin{code}
{-# language GADTs #-}
{-# language DataKinds, TypeOperators #-}
{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
import qualified Control.Monad.Freer as F
import qualified Control.Monad.Freer.Internal as FI
import qualified Control.Monad.Freer.Reader as FR
import Data.OpenUnion ((:++:))
import Control.Distributed.Process (ProcessId, liftIO, Process, spawnLocal)
\end{code}
\begin{code}
main = undefined
\end{code}

\begin{code}
newtype PEff r fr a = PEff (F.Eff (Proc ': r) a) deriving (Functor, Applicative, Monad)
\end{code}


Gebruik maken van lists voor non-determinism?
Op deze manier kunnen we stappelen en dan een andere interpreter gebruiken voor de Process events, iederen van wie weer binnen de rest moet draaien.
   
\begin{code}
runPEff :: (F.LastMember Process r) => PEff r rf a -> F.Eff r a
runPEff (PEff eff) = F.interpretM f eff
  where
   f :: (Proc r a -> Process a)
   f (Spawn s) = (spawnLocal _)
\end{code}

\begin{code}

type PID = ProcessId

data Proc r a where
  Spawn :: (F.Members r' r) => PEff r' fr' () -> Proc r PID
  Call  :: (F.Members r' r) => PEff r' fr' a -> Proc r a
  Ask   ::  Proc r (PEff r rf a -> a)
  Send :: a -> Proc r ()
  Say  :: String -> Proc r ()
  Expect :: Proc r a
\end{code}

\begin{code}
\end{code}

\begin{code}

spawn :: (F.Members r' r) => PEff r' rf' () -> PEff r rf PID
spawn p = send (Spawn p)

ask :: PEff r rf (PEff r rf a -> a)
ask = send Ask

send :: (F.Member eff ((Proc effs) ': effs)) => eff a -> PEff effs rf a
send t = PEff (FI.E (FI.inj t) (FI.tsingleton FI.Val))



\end{code}

\begin{code}
lift :: F.Eff r a -> PEff r rf a
lift = upgrade . F.raise
\end{code}

\begin{code}
upgrade :: F.Eff (Proc r ': r) a -> PEff r rf a
upgrade = PEff
\end{code}

Try to factor out the runPEff, maybe using package/existential types
Use call for runPEff?
\begin{code}
raise :: (F.LastMember Process r) => PEff r fr a -> PEff (e ': r) fr a
raise p = PEff (F.raise (F.raise (runPEff p)))
\end{code}
