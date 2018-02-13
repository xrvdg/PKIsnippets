\begin{code}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE RecursiveDo #-}
  module Control (
    controlView
  )
  where

  import qualified Data.Text as T
  import Graphics.UI.Threepenny as GUT
  import qualified Forms.AddRemove as ARF
  import Control.Concurrent (threadDelay)
  import App
  import TPGProcess
  import Control.Monad.Identity
\end{code}


Operations
The forms are used for multiple operations in controlView these form get a meaning assigned to them.

Where in ADD the second is dependent on the first.


\begin{code}
  data Operation = Select String | Remove String |
                   AddLink String String | RmLink String String |
                   AddRel String String | RmRel String String deriving Show

  delayedShow :: (Show a) => a -> ProcessNonUI () String
  delayedShow a = liftIO (threadDelay delay) >> return  (show a ++ show delay)
    where delay = 5000000

  controlView :: AppT UI Element
  controlView = do
                   regular <- liftUI $ ARF.mkAddRemove "regular"
                   status <-  liftUI $ string "No status yet"
                   onEventProcess (ARF.ev regular) (const ()) delayedShow (\b -> element status # set text b)
                   liftUI $ do
                               bCurrent <- stepper Nothing (ARF.getText ARF.isADD <$> ARF.ev regular)
                               parent <- ARF.mkAddRemove "parent"
                               child <- ARF.mkAddRemove "child"
                               relation <- ARF.mkAddRemove "relation"
                               new #+ [element status, element regular, element parent, element child, element relation]

\end{code}
