-- Form


-- [[file:~/projecten/PKI/src/Control.org::*Form][Form:1]]
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
module Forms.AddRemove (
  isADD,
  isRM,
  getText,
  mkAddRemove,
  ev
)
where

import Graphics.UI.Threepenny
import Debug
import GHC.Generics
import Data.Binary
import Data.Typeable

data ADDRM = ADD String | RM String deriving (Show, Generic, Binary, Typeable)


isADD :: ADDRM -> Bool
isADD (ADD e) = True
isADD _ = False

isRM :: ADDRM -> Bool
isRM (RM e) = True
isRM _ = False

getText :: (ADDRM -> Bool) -> ADDRM -> Maybe String
getText p op = if p op then Just (getText' op) else Nothing

getText' :: ADDRM -> String
getText' (ADD str) = str
getText' (RM str) = str

data AddRemoveForm = ARF {elemARF :: Element, ev :: Event ADDRM}

mkAddRemove :: String -> UI AddRemoveForm
mkAddRemove title = mdo 
                         f <- new # set class_ "addremove"
                         header <- string title 

                         add <- button # set text "add" 
                         remove <- button # set text "remove" 
                         let eadd = click add
                             eremove = click remove

                         uinput <- entry binput
                         let einput = rumors $ userText uinput
                         binput <- stepper "" einput 

                         let eoutput = head <$> unions [ADD <$> binput <@ eadd, RM <$> binput <@ eremove]

                         lastAction <- stepper "" (show <$> eoutput)

                         db <- debugString "Nothing"
                         sink text lastAction (element db)

                         element f #+ [element header, element uinput, element add, element remove, element db]
                         return (ARF f eoutput)

instance Widget AddRemoveForm where
  getElement = elemARF
-- Form:1 ends here
