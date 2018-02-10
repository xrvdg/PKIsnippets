\begin{code}
module Debug (
  debugString
)
where

-- | This module contains tooling to ease development of the GUI.

import Graphics.UI.Threepenny

debugString :: String -> UI Element
debugString t = string t # set class_ "debug"
\end{code}
