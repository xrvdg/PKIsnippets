\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Graphics.UI.Threepenny as GUT hiding (register)
import TPGEff
import Control.Monad.Freer as F
import Control.Monad.Freer.Proc as FP
import Control.Monad.Freer.Handler as FH
import Data.HVect
import qualified Data.Text as T
import Control.Monad
import Data.ByteString
import qualified Control.Distributed.Process as DP
import Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node as Node
import Control

\end{code}

\begin{code}
main :: IO ()
main = startApp setup

setup :: (ProcConstraint n '[] '[]) => Window -> Eff '[TPGEff.UI, Proc '[]] ()
setup w = do
  cv <- controlView
  void $ TPGEff.liftUI (do
    return w # set title "test neo4j"
    getBody w #+ [element cv])
\end{code}
\begin{code}
startApp :: (Window -> Eff '[TPGEff.UI, Proc '[]] ()) -> IO ()
startApp appui = do
  backend <- initializeBackend "127.0.0.1" "8230" Node.initRemoteTable
  node <- newLocalNode backend
  let config = defaultConfig {jsStatic = Just ".",
                              jsPort = Just 8200,
                              jsLog = logProcess node}
  startGUI config (\w -> GUT.liftIO $ FP.runProcess node HNil (TPGEff.runUI w $ appui w))
\end{code}

We define our own logger for threepenny gui such that there cannot be race conditions for stderr.
It seems that snap, the server underlying threepenny gui, does not use the logger when it firsts boots up.
All the later messages do seem to use it. Looking into the source of threepenny gui seems that threepenny gui does initialize the logger of snap correctly. The best thing would probably be to not use say in any process that starts before startGUI.

\begin{code}
logProcess :: Node.LocalNode -> ByteString -> IO ()
logProcess node bs = Node.runProcess node (DP.say ("say: " ++ show bs))
\end{code}
