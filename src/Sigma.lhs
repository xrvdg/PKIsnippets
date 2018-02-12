\begin{code}
  {-# LANGUAGE OverloadedStrings #-}
  module Sigma
   (createSigma)
  where
  import Graphics.UI.Threepenny as GUT
  import Data.Aeson
  import qualified Data.Text as T
  import Data.Monoid ((<>))
\end{code}

\begin{code}
  -- sigmajs
  -- [[file:~/projecten/PKI/src/Main.org::*sigmajs][sigmajs:1]]
  createSigma :: UI Element
  createSigma = do
    g <- GUT.div # set id_ "scontainer"
    graph <- GUT.div # set id_ "sg" # set style [("margin", "auto")]
    s <- string "Hello world"
    b <- button # set text "Appear"

    let js = show (encode testSG)
    let command = ("s.graph.clear();\n" <>
                   "s.graph.read(" ++ js ++ ");\n" <>
                   "s.refresh();\n" <>
                   "s.startNoverlap();")

    on click b (\a -> runFunction $ ffi command)

    sgm <- mkElement "script" # set (attr "src") "/static/sigma.js/build/sigma.min.js"
    sset <- mkElement "script" # set (attr "src") "/static/src/sig.js"
    slayout <- mkElement "script" # set (attr "src") "/static/sigma.js/build/plugins/sigma.layout.noverlap.min.js"
    sani <- mkElement "script" # set (attr "src") "/static/sigma.js/build/plugins/sigma.plugins.animate.min.js"

    st <- mkElement "style" # set (attr "type") "text/css" # set html  "#sg {max-width: 400px; height: 400px; margin: auto;}"
    element g #+ [element st, element graph, element s, element sgm, element sani, element slayout, element sset, element b]
  -- sigmajs:1 ends here

  -- sigma.js
  -- We maken een data structuur voor sigma.js die we eenvoudig van en naar JSON kunnen maken.
  -- Hoewel we waarschijnlijk ongerichte grafen blijven tekenen, kent sigmajs wel source and target id en wij hanteren het ook om tijdens het coden consistent te blijven.


  -- [[file:~/projecten/PKI/src/Main.org::*sigma.js][sigma.js:1]]
  data SG = SG [SNode] [SEdge]

  type ID = Int
  type Label = T.Text

  type SourceID = ID
  type TargetID = ID

  data SNode = SN ID Label
  data SEdge = SE ID SourceID TargetID

  instance ToJSON SG where
    toJSON (SG n e) = object ["nodes" .= toJSON n, "edges" .= toJSON e]

  instance ToJSON SEdge where
    toJSON (SE id source target) = object ["id" .= id, "source" .= source, "target" .= target]

  instance ToJSON SNode where
    toJSON (SN id label) = object ["id" .= id, "label" .= label, "x" .= (20 :: Int), "y" .= (30 :: Int), "size" .= (10 :: Int)]
  -- sigma.js:1 ends here



  -- De nodes hebben wel echt een positie nodig anders worden ze niet getekend. Ook wanneer er sprake is van nooverlap
  -- Het maken van een node moet pas gebeuren wanneer de browser geheel is geladen.

  -- In het voorbeeld op de site maken ze gebruik van aparte identifiers voor edges en nodes. We hanteren eerst Ints en zien wel of we ze ook op dit level moeten onderscheiden.


  -- [[file:~/projecten/PKI/src/Main.org::*sigma.js][sigma.js:2]]
  testSG :: SG
  testSG = SG [SN 1 "Henk", SN 2 "Frits", SN 3 "Barend"] [SE 1 1 2]
  -- sigma.js:2 ends here
\end{code}
