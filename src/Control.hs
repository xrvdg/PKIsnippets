-- imports etc

-- [[file:~/projecten/PKI/src/Control.org::*imports%20etc][imports etc:1]]
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Control (
  controlView
)
where

import qualified Data.Text as T
import Graphics.UI.Threepenny as GUT
import Debug
import qualified Forms.AddRemove as ARF
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Control.Distributed.Process
import Control.Concurrent (threadDelay)
import Control.Monad (void)
-- imports etc:1 ends here

-- Operations
-- The forms are used for multiple operations in controlView these form get a meaning assigned to them.

-- Where in ADD the second is dependent on the first.


-- [[file:~/projecten/PKI/src/Control.org::*Operations][Operations:1]]
{-
||| A process server that has a single task.
-}
processTask :: Serializable a => (a -> IO b) -> Process ()
processTask f = do a <- expect
                   liftIO (f a)
                   processTask f

{-
||| A process server with a single tasks that executes handler when the function is done.
-}
handlerProcessTask :: Serializable a => GUT.Handler b -> (a -> IO b) -> Process ()
handlerProcessTask h f = processTask (\a -> f a >>= h)

onEventProcess :: Serializable a => LocalNode -> Event a -> (a -> IO b) -> (b -> UI void) -> UI ()
onEventProcess nid event rf gf = void $  do 
       (callbackev, fire) <- liftIO newEvent 
       pid <- liftIO $ forkProcess nid (handlerProcessTask fire rf)
       onEvent event (\a -> liftIO . runProcess nid $ send pid a)
       onEvent callbackev gf

data Operation = Select String | Remove String | 
                 AddLink String String | RmLink String String | 
                 AddRel String String | RmRel String String deriving Show

controlView :: LocalNode -> UI Element
controlView ln = do 
                 regular <- ARF.mkAddRemove "regular"
                 bCurrent <- stepper Nothing (ARF.getText ARF.isADD <$> ARF.ev regular)
                 parent <- ARF.mkAddRemove "parent"
                 child <- ARF.mkAddRemove "child"
                 relation <- ARF.mkAddRemove "relation"
                 status <- string "No status yet"
                 let delay = 10000000
                 onEventProcess ln (ARF.ev regular) (\a -> threadDelay delay >> return  (show a ++ show delay)) (\b -> element status # set text b)
                 new #+ [element status, element regular, element parent, element child, element relation]
-- Operations:1 ends here
