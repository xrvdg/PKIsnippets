-- Debug
-- :PROPERTIES:
-- :header-args: :tangle ./Debug.hs :comments both
-- :END:

-- This module contains tooling to ease development of the GUI.


-- [[file:~/projecten/PKI/src/Debug.org::*Debug][Debug:1]]
module Debug (
  debugString
)
where

import Graphics.UI.Threepenny

debugString :: String -> UI Element
debugString t = string t # set class_ "debug"
-- Debug:1 ends here
