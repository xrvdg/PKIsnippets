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

  settings = def {B.user="neo4j", B.password="pki2018"}
\end{code}

\begin{code}
  main :: IO ()
  main = startApp settings setup

  setup :: Window -> AppT UI ()
  setup w = do
    cv <- controlView
    liftIO . runUI w . void $ do
      return w # set title "test neo4j"
      sigmael <- createSigma
      getBody w #+ [element cv]
\end{code}
