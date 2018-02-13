TODO: This section is probably better named as Graphics.UI.Threepenny.Process
\begin{code}
{-# LANGUAGE MonoLocalBinds, DeriveDataTypeable, DeriveGeneric #-}
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
  import Control.Distributed.Process.Node
  import Control.Distributed.Process.Serializable
  import Control.Distributed.Process
  import Control.Monad (void)
  import Process
\end{code}

These typeclasses are used to restricted what can be executed.
The HasWindow typeclass is for being able to construct liftUI instances and that we can split up the state of an app in parts that are and arenot UI.
The HasLocalNode is used to inform the compiler that these datatypes have a localNode and therefore be used onEventProcess.

Can probably be combined into a single class, but still have the same methods.
Will clean up the onEventProcess signamture.

\begin{code}

  class TPGProcessInfo a where
    localNode :: a -> LocalNode
    window :: a -> Window

  instance (MonadUI m, MonadIO m, TPGProcessInfo w) => MonadUI (ReaderT w m) where
    liftUI uia = do w <- asks window
                    liftIO $ runUI w (liftUI uia)
\end{code}
Process servers are processes that run an IO computation when it receives the expected input.

\begin{code}
  -- | A process server that has a single task.
  processTask :: Serializable a => (a -> Process b) -> Process ()
  processTask f = do a <- expect
                     say $ "received: "
                     f a
                     processTask f

  -- | A process server with a single tasks that executes handler when the function is done.
  handlerProcessTask :: Serializable a => GUT.Handler b -> (a -> Process b) -> Process ()
  handlerProcessTask h f = processTask (\a -> f a >>= liftIO . h)

\end{code}

Here we require a function which is able to extract the required surroudning/config for its ReaderT from ss.
These will general be of the form runProcessInContext :: ServerState -> a -> ProcessNonUI

ss -> (a -> ProcessNonUI cb) -> (a -> Process b)

TODO: Probably can make a datatype which captures the TPGProcessInfo in its type such that it does not have to be repeated here.
-> some ReaderT construct.
\begin{code}
  -- | In usage it acts the same as onEvent but rather than running an IO operation in GUI thread it launches a process server for this IO operation.
  onEventProcess  :: (Serializable a, TPGProcessInfo ss) => Event a -> (ss -> c) -> (a -> ProcessNonUI c b) -> (b -> UI void) -> ReaderT ss UI ()
  onEventProcess event extract rf gf = void $  do
         (callbackev, fire) <- liftIO newEvent
         cs <- asks extract
         nid <- asks localNode
         pid <- liftIO $ forkProcess nid (handlerProcessTask fire (\a -> runReaderT (rf a) cs))
         liftUI $ do
                     onEvent event (liftIO . runProcess nid . send pid)
                     onEvent callbackev gf
\end{code}

\begin{code}
  -- | To be used in combination with ProcessT to annotate that code is not allowed to use UI.
  data NonUI

  type ProcessNonUI a b = PProcessT NonUI a b
\end{code}

TODO: introduce a class instance for onEventProcess that extracts the desired state from the r carried around in ReaderT. Then the current onEventProcess can have a extra function argument to make the type stricter.
Might also require to split the Window/UI object.

At the moment of this writing only Window and Pool Pipe are data inside AppT that should be accessed.

Maybe requires an extra datatype which acts as a way of describing the allowed operations on the state?
This probably results in having an extra type besides AppT in which AppT will really our application information and a different datatype to carry these general properties.

Probably also use this splitting of the datatype to enforce what does and doesn't have access to the liftUI functionality.
