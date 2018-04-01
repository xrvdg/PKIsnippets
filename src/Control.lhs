\begin{code}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE RecursiveDo #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE MonoLocalBinds #-}
  module Control (
    controlView
  )
  where

  import qualified Data.Text as T
  import Graphics.UI.Threepenny as GUT
  import qualified Forms.AddRemove as ARF
  import Control.Monad.Freer
  import Control.Monad.Freer.Proc as FP
  import Control.Concurrent (threadDelay)
  import Control.Monad (void)
  import TPGEff
\end{code}


Operations
The forms are used for multiple operations in controlView these form get a meaning assigned to them.

Where in ADD the second is dependent on the first.


\begin{code}
  data Operation = Select String | Remove String |
                   AddLink String String | RmLink String String |
                   AddRel String String | RmRel String String deriving Show

  delayedShow :: (Show a) => a -> Eff '[Proc '[]] String
  delayedShow a = FP.liftIO (threadDelay delay) >> return (show a ++ show delay)
    where delay = 5000000

  controlView :: (Member TPGEff.UI effs, HasProc n r effs) => Eff effs Element
  controlView = do
                   regular <- TPGEff.liftUI $ ARF.mkAddRemove "regular"
                   status <-  TPGEff.liftUI $ string "No status yet"
                   onEventProcess (ARF.ev regular) delayedShow (\b -> void $ element status # set text b)
                   TPGEff.liftUI $ do
                               bCurrent <- stepper Nothing (ARF.getText ARF.isADD <$> ARF.ev regular)
                               parent <- ARF.mkAddRemove "parent"
                               child <- ARF.mkAddRemove "child"
                               relation <- ARF.mkAddRemove "relation"
                               new #+ [element status, element regular, element parent, element child, element relation]

\end{code}
