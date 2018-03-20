TODO: This section is probably better named as Graphics.UI.Threepenny.Process
\begin{code}
{-# LANGUAGE MonoLocalBinds, DeriveDataTypeable, DeriveGeneric #-}
{-# language DataKinds #-}
\end{code}
\begin{code}
  module TPGProcess(
    onEventProcess,
    ProcessNonUI,
    NonUI,
    TPGProcessInfo(..)

)
  where

  import Graphics.UI.Threepenny as GUT
  import Control.Monad.Reader
  import Data.Binary
  import Control.Distributed.Process.Serializable
  import Control.Monad (void)
  import Process
  import Control.Monad.Freer
\end{code}

Process servers are processes that run an computation when it receives thf LtU to new servers is complete.

If you notice any issues with the site, please post in this thread (if you can), or email me at antonvs8 at (gmail domain).

Original announcement appears below:e expected input.

TODO: determine if this processTask is nicely without the 'Let it crash' philosophy or that it should be a slightly more fault tolerant.
Might involve changing the handler, because the handler is the precious part. Losing the handler means losing the connection to corresponding TPG-event receiver.
Here we want to let them both have the same effects
\begin{code}
  -- | A process server that has a single task.
  processTask :: (Member Process effs, Serializable a) => (a -> Eff effs b) -> Eff effs ()
  processTask f = do a <- expect
                     say $ "received: "
                     f a
                     processTask f

  -- | A process server with a single tasks that executes handler when the function is done.
  handlerProcessTask :: (Member Process effs, Serializable a) => GUT.Handler b -> (a -> Eff effs b) -> Eff effs ()
  handlerProcessTask h f = processTask (\a -> f a >>= liftIO . h)

\end{code}

TODO: Probably can make a datatype which captures the TPGProcessInfo in its type such that it does not have to be repeated here.
-> some ReaderT construct. -> Also requires GADT

TODO: This defininition for onEvent is not very nice yet. Come back to this after the GADT/Dependent types lectures.
This at least gives a general structure to express the idea.
A proper solution will probably extract the resource constraining of a process to the process module.

TODO: probably need to create an onEvent that takes an eff as argument rather than IO.
TODO: maybe this doesn't need an Eff effs -> No it does because otherwise we still need to give it the localnode to run.
\begin{code}
  -- | In usage it acts the same as onEvent but rather than running an IO operation in GUI thread it launches a process server for this IO operation.
  onEventProcess  :: (Serializable a, Member Process effs', Members '[Process, UI] effs) => Event a -> (a -> Eff effs b) -> (b -> UI void) -> Eff effs ()
  onEventProcess event extract rf gf = void $  do
         (callbackev, fire) <- liftIO newEvent
         cs <- asks extract
         nid <- asks localNode
         pid <- spawn (handlerProcessTask fire (\a -> runReaderT (rf a) cs))
         sendM $ do
                     onEvent event (send pid)
                     onEvent callbackev gf
\end{code}

TODO: make  the types stronger such that
TODO: introduce a class instance for onEventProcess that extracts the desired state from the r carried around in ReaderT. Then the current onEventProcess can have a extra function argument to make the type stricter.
Might also require to split the Window/UI object.

At the moment of this writing only Window and Pool Pipe are data inside AppT that should be accessed.

Maybe requires an extra datatype which acts as a way of describing the allowed operations on the state?
This probably results in having an extra type besides AppT in which AppT will really our application information and a different datatype to carry these general properties.

Probably also use this splitting of the datatype to enforce what does and doesn't have access to the liftUI functionality.
