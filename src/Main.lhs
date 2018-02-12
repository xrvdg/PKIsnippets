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
  import Data.Pool
  import App

  -- Neo4j
  -- Start with setting up a network connection and then have a server which can receive queries
  settings = def {B.user="neo4j", B.password="pki2018"}

  doquery :: IO ()
  doquery = do
    pipe <- B.connect settings
    let q = B.query "match (a) return a limit 4;"
    res <- B.run pipe q
    print res
    let first = head res
    bla <- first `B.at` "movie" >>= B.exact :: IO B.Node
    print bla



\end{code}



\begin{code}
  main :: IO ()
  main = startApp settings setup

  setup :: Window -> AppT UI ()
  setup w = do
    cv <- controlView w
    liftIO . runUI w . void $ do
      return w # set title "test neo4j"
      sigmael <- createSigma
      getBody w #+ [element cv]
\end{code}
