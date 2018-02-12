\begin{code}
  {-# LANGUAGE OverloadedStrings #-}
  module Main where
  import Data.Default (def) -- | Required for default by HasBolt
  import qualified Database.Bolt as B
  import Graphics.UI.Threepenny as GUT hiding (register)
  import qualified Data.Text as T
  import Control.Monad
  import qualified Data.ByteString.Lazy.Char8 as BS
  import qualified Data.ByteString as BSI
  import Data.Monoid ((<>))
  import Control
  import Control.Concurrent (threadDelay)
  import Control.Distributed.Process
  import Control.Distributed.Process.Backend.SimpleLocalnet
  import qualified Control.Distributed.Process.Node as Node
  import System.Environment
  import Control.Monad.Reader
  import Data.Maybe
  import Sigma

  -- Neo4j
  -- Start with setting up a network connection and then have a server which can receive queries
  settings = def {B.user="neo4j", B.password="pki2018"}

  db :: Process ()
  db = do pipe <- B.connect settings
          queryHandler pipe

  queryHandler :: B.Pipe -> Process ()
  queryHandler pipe = undefined
{-  do
                 (pid, query) <- expect
                 B.query query
                 res <- B.run pipe
                 send pid res
                 queryHandler pipe
   -}


  doquery :: IO ()
  doquery = do
    pipe <- B.connect settings
    let q = B.query "match (a) return a limit 4;"
    res <- B.run pipe q
    print res
    let first = head res
    bla <- first `B.at` "movie" >>= B.exact :: IO B.Node
    print bla
  -- Neo4j:1 ends here

  -- GUI
  -- :PROPERTIES:
  -- :header-args: :tangle ./Main.hs :comments both
  -- :END:

  -- We define our own logger for threepenny gui such that there cannot be race conditions for stderr.
  -- It seems that snap, the server underlying threepenny gui, does not use the logger when it firsts boots up.
  -- All the later messages do seem to use it. Looking into the source of threepenny gui seems that threepenny gui does initialize the logger of snap correctly.

  -- The best thing would probably be to not use say in any process that starts before startGUI.

  -- [[file:~/projecten/PKI/src/Main.org::*GUI][GUI:1]]
  logProcess :: Node.LocalNode -> BSI.ByteString -> IO ()
  logProcess node bs = Node.runProcess node (say ("say: " ++ show bs))


\end{code}

DUI is a wrapper around the UI monad to hold information about the localNode on which processes are spawned and the configuration on which we use.

The configuration is currently empty but is already defined here as a placeholder. In the future it will likely contain information about which programs to use.
Eventhough that might also be delegated to 'xdg-open'.

\begin{code}
  type DUI = ReaderT ServerState UI
  data ServerState = SS ProcessState Main.Config
  data Config
  data ProcessState = PS Node.LocalNode
\end{code}

  TPG has the convention that if the binding address is Nothing it will check look for the environment variable 'ADDR', if that isn't found then '127.0.0.1' is used.
  We use the same logic for our the address of our Neo4j server.
\begin{code}
  getAddr :: IO String
  getAddr =  fmap (fromMaybe "127.0.0.1") (lookupEnv "ADDR")

  main :: IO ()
  main = do
    addr <- getAddr
    backend <- initializeBackend addr "8030" Node.initRemoteTable
    node <- newLocalNode backend
    let config = defaultConfig {jsStatic = Just ".",
                                jsAddr = Just (BS.toStrict (BS.pack addr)),
                                jsPort = Just 8200,
                                jsLog = logProcess node}
    void $ Node.runProcess node $ do
               say "starting db"
               dbpid <- spawnLocal db
               register "db" dbpid
               say "starting startGUI"
               liftIO $ startGUI config (setup node)

  setup :: Node.LocalNode -> Window -> UI ()
  setup nid w = void $ do
    return w # set title "test neo4j"
    sigmael <- createSigma
    getBody w #+ [element sigmael, controlView nid]

\end{code}
