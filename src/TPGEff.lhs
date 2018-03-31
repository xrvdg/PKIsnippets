\begin{code}
{-# language FlexibleContexts #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language GADTs #-}
{-# language ConstraintKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
module TPGEff () where
import qualified Graphics.UI.Threepenny as TPG
import Control.Monad.Freer
import Control.Monad (void)
import qualified Control.Monad.Freer.Proc as FP
import Control.Distributed.Process.Serializable (Serializable)
import qualified Control.Distributed.Process as DP
\end{code}

\begin{code}
  -- | A process server that has a single task.
fix :: (a -> a) -> a
fix f = let {x = f x} in x

processTask :: (
  Serializable a,
  FP.ProcConstraint n s s
   )
     => TPG.Handler b -> (a -> Eff s b) -> Eff '[FP.Proc s] ()
processTask h f = fix (\pt h f ->
                do a <- FP.expect
                   FP.say $ "received: "
                   b <- FP.call (f a)
                   FP.liftIO (h b)
                   pt h f) h f
\end{code}
\begin{code}
data UI a where
  LiftUI :: TPG.UI a -> UI a

liftUI :: (Member UI effs) => TPG.UI a -> Eff effs a
liftUI = send . LiftUI

\end{code}
\begin{code}
onEventProcess  :: forall a n s r effs b. (FP.ProcConstraint n '[FP.Proc s] r, FP.ProcConstraint n s s, Serializable a, Member UI effs, FP.ProcConstraint n s r, LastMember (FP.Proc r) effs) => TPG.Event a -> (a -> Eff s b) -> (b -> TPG.UI ()) -> Eff effs ()
onEventProcess event handler callback = void $  do
       (callbackev, fire) <- FP.liftIO TPG.newEvent
       let pt = processTask fire handler
       pid <- FP.spawn pt
       sio <- FP.runIO (FP.send pid)
       liftUI $ do
                   TPG.onEvent event (TPG.liftIO . sio)
                   TPG.onEvent callbackev callback
\end{code}

