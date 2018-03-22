\begin{code}
{-# language GADTs #-}
{-# language DataKinds, TypeOperators #-}
{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
import qualified Control.Monad.Freer as F
import qualified Control.Monad.Freer.Reader as FR
import Control.Distributed.Process (ProcessId, liftIO, Process, spawnLocal)
\end{code}
\begin{code}
main = undefined
\end{code}

\begin{code}
newtype PEff r fr a = PEff (F.Eff r a) deriving (Functor, Applicative, Monad)
\end{code}


\begin{code}
runPEff :: (F.LastMember Process r) => PEff ((Proc r) ': r) rf a -> PEff r rf a
runPEff (PEff eff) = PEff (F.interpretM f eff)
  where
   f :: (Proc r a -> Process a)
   f (Spawn r s) = (spawnLocal _)
\end{code}

\begin{code}
data Proc r a where
  Spawn :: (forall a rf. PEff r rf a -> a) -> PEff r fr () -> Proc r ProcessId
  Call  ::  (forall a rf. PEff r rf a -> a) -> PEff r fr a -> Proc r a
  Ask   ::  Proc r (PEff r rf a -> a)
  Send :: a -> Proc r ()
  Say  :: String -> Proc r ()
  Expect :: Proc r a
\end{code}

\begin{code}
\end{code}

\begin{code}
--spawn :: (F.Members e r) => PEff e rf () -> PEff r rf ProcessId
--spawn eff = do
--  r <- ask
--  send (Spawn r eff)

ask :: PEff r rf (PEff r rf a -> a)
ask = send Ask

send :: (F.Member eff effs) => eff a -> PEff effs rf a
send = lift . F.send

\end{code}

\begin{code}
lift :: F.Eff r a -> PEff r rf a
lift = PEff
\end{code}

\begin{code}
raiseProc :: (F.LastMember Process r) => PEff ((Proc r) ': r) fr a -> PEff ( (Proc (e ': r)) ': e ': r) fr a
raiseProc p = raise (raise (runPEff p))
\end{code}

\begin{code}
raise :: PEff r fr a -> PEff (e ': r) fr a
raise (PEff eff) = PEff (F.raise eff)
\end{code}
