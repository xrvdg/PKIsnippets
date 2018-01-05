-- Imports 

-- [[file:~/projecten/PKI/src/Main.org::*Imports][Imports:1]]
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Default
import qualified Database.Bolt as B
import Graphics.UI.Threepenny as GUT
import qualified Data.Text as T
import Control.Monad 
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Monoid ((<>))
-- Imports:1 ends here

-- Neo4j


-- [[file:~/projecten/PKI/src/Main.org::*Neo4j][Neo4j:1]]
doquery :: IO ()
doquery = do
  pipe <- B.connect $ def {B.user="neo4j", B.password="pki2018"}
  let q = B.query "match (movie:Movie) return movie limit 4;"
  res <- B.run pipe q
  print res
  let first = head res
  bla <- first `B.at` "movie" >>= B.exact :: IO B.Node
  print bla
-- Neo4j:1 ends here

-- GUI
-- :PROPERTIES:
-- :header-args: :tangle ./Main.hs :comments both
-- :END:


-- [[file:~/projecten/PKI/src/Main.org::*GUI][GUI:1]]
main :: IO ()
main = startGUI defaultConfig {jsStatic = Just ".", jsPort = Just 8200} setup

setup :: Window -> UI ()
setup w = void $ do
  return w # set title "test neo4j"
  graph <- GUT.div # set id_ "sg" # set style [("margin", "auto")]
  s <- string "Hello world"

  sgm <- mkElement "script" # set (attr "src") "/static/sigma.js/build/sigma.min.js"
  sp <- mkElement "script" # set (attr "src") "/static/sigma.js/build/plugins/sigma.parsers.json.min.js"

  bla <- mkElement "script" # set html (" sigma.parsers.json('/static/data.json', {" <>
    "container: 'sg'," <>
    "settings: {" <>
    "defaultNodeColor: '#ec5148'}});")

  st <- mkElement "style" # set (attr "type") "text/css" # set html  "#sg {max-width: 400px; height: 400px; margin: auto;}"
  let js = BS.unpack $ encode testSG
  sh <- mkElement "script" # set html ("var g = " ++ js ++ ";\n var s = new sigma({graph: g, container: 'sg'});\n")
  getBody w #+ [element s, element sgm, element sp, element graph, element bla]
  getHead w #+ [element st]
  return ()
-- GUI:1 ends here

-- Datastructures
-- :PROPERTIES:
-- :header-args: :tangle ./Main.hs :comments both
-- :END:


-- [[file:~/projecten/PKI/src/Main.org::*Datastructures][Datastructures:1]]
data Movie = Movie {tit :: T.Text, released :: Int}
data Person = Person {name :: T.Text, born :: Int}
-- Datastructures:1 ends here

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
  toJSON (SE id source target) = object ["id" .= ("e" ++ show id), "source" .= ("n"++ show source), "target" .= ("n" ++ show target)]

instance ToJSON SNode where
  toJSON (SN id label) = object ["id" .= ("n" ++ show id), "label" .= label, "color" .= ("#ccc" :: T.Text), "x" .= (20 :: Int), "y" .= (30 :: Int), "size" .= (10 :: Int)]
-- sigma.js:1 ends here



-- In het voorbeeld op de site maken ze gebruik van aparte identifiers voor edges en nodes. We hanteren eerst Ints en zien wel of we ze ook op dit level moeten onderscheiden.


-- [[file:~/projecten/PKI/src/Main.org::*sigma.js][sigma.js:2]]
testSG :: SG
testSG = SG [SN 1 "Henk", SN 2 "Frits", SN 3 "Barend"] [SE 1 1 2]
-- sigma.js:2 ends here
