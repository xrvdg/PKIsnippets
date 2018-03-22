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
newtype PEff r fr a = PEff (F.Eff (Proc r ': r) a) deriving (Functor, Applicative, Monad)
\end{code}


\begin{code}
runPEff :: (F.LastMember Process r) => PEff r rf a -> F.Eff r a
runPEff (PEff eff) = F.interpretM f eff
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
ask = sendProc Ask

sendProc :: Proc r a -> PEff r rf a
sendProc eff = _

send :: (F.Member eff effs) => eff a -> PEff effs rf a
send = lift . F.send

\end{code}

\begin{code}
lift :: F.Eff r a -> PEff r rf a
lift = PEff . F.raise
\end{code}

\begin{code}
upgrade :: F.Eff (Proc r ': r) a -> PEff r rf a
upgrade = PEff
\end{code}

\begin{code}
raise :: (F.LastMember Process r) => PEff r fr a -> PEff (e ': r) fr a
raise p = PEff (F.raise (F.raise (runPEff p)))
\end{code}
