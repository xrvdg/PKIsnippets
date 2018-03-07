\begin{code}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
\end{code}
\begin{code}
  module App (
    startApp,
    AppT,
    localNode,
    Addr,
    ask,
    asks,
    runReaderT
  )
  where
\end{code}

\begin{code}
  import Graphics.UI.Threepenny as GUT
  import Control.Monad.Reader
  import Control.Distributed.Process
  import qualified Control.Distributed.Process.Node as Node
  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Char8 as BSC
  import Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend, newLocalNode)
  import Data.Pool
  import Database.Bolt
  import Data.Maybe (fromMaybe)
  import System.Environment (lookupEnv)
  import Process
  import TPGProcess
\end{code}
    }

The configuration is currently empty but is already defined here as a placeholder. In the future it will likely contain information about which programs to use; although we might delegate that to 'xdg-open'.

\begin{code}
  type AppT = ReaderT ServerState

  data ServerState = SS {_ps :: ProcessState, _w :: Window}


  data ProcessState = PS {_ln :: Node.LocalNode, _p :: Pool Pipe}

  instance TPGProcessInfo ServerState where
    localNode = _ln . _ps
    window = _w

  pool :: ServerState -> Pool Pipe
  pool = _p . _ps


\end{code}

  TPG has the convention that if the binding address is Nothing it will check look for the environment variable 'ADDR', if that isn't found then '127.0.0.1' is used.
  We use the same logic for our the address of our Neo4j server.

\begin{code}
  type Addr = String

  -- | Check if the 'ADDR' environment variable exists, and if not return "127.0.0.1"
  getAddr :: IO Addr
  getAddr =  fmap (fromMaybe "127.0.0.1") (lookupEnv "ADDR")
\end{code}

Rewrite this to something that has a state which also incorperate the window.
Window is also a state in this application and therefore should also be in the read.

Model this after startGUI.
\begin{code}
  startApp :: BoltCfg -> (Window -> AppT UI ()) -> IO ()
  startApp bcfg appui = do
    addr <- getAddr
    backend <- initializeBackend addr "8230" Node.initRemoteTable
    node <- newLocalNode backend
    rpool <- createPool (connect bcfg) close 4 500 1
    let config = defaultConfig {jsStatic = Just ".",
                                jsAddr = Just (BSC.pack addr),
                                jsPort = Just 8200,
                                jsLog = logProcess node}
        serverstate = SS (PS node rpool)
    Node.runProcess node . liftIO $ startGUI config (\w -> runReaderT (appui w) (serverstate w))
\end{code}

We define our own logger for threepenny gui such that there cannot be race conditions for stderr.
It seems that snap, the server underlying threepenny gui, does not use the logger when it firsts boots up.
All the later messages do seem to use it. Looking into the source of threepenny gui seems that threepenny gui does initialize the logger of snap correctly. The best thing would probably be to not use say in any process that starts before startGUI.

\begin{code}
  logProcess :: Node.LocalNode -> BS.ByteString -> IO ()
  logProcess node bs = Node.runProcess node (say ("say: " ++ show bs))
\end{code}

An example of a pre-processor is to convert your source files to the input
