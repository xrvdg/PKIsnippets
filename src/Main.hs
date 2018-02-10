-- Imports 

-- [[file:~/projecten/PKI/src/Main.org::*Imports][Imports:1]]
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main where
import Data.Default
import qualified Database.Bolt as B
import Graphics.UI.Threepenny as GUT hiding (register)
import qualified Data.Text as T
import Control.Monad 
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as BSI
import Data.Monoid ((<>))
import Control
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node as Node
-- Imports:1 ends here

-- Neo4j
-- Start with setting up a network connection and then have a server which can receive queries

-- [[file:~/projecten/PKI/src/Main.org::*Neo4j][Neo4j:1]]
settings = def {B.user="neo4j", B.password="pki2018"}

db :: Process ()
db = do pipe <- B.connect settings
        queryHandler pipe

queryHandler :: Process ()
queryHandler pipe = do 
               (pid, query) <- expect
               B.query query
               res <- B.run pipe
               send pid res
               queryHandler pipe
               
               

doquery :: IO ()
doquery = do
  pipe <- B.connect settings
  let q = B.query "match (a) return a limit 4;"
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

-- We define our own logger for threepenny gui such that there cannot be race conditions for stderr.
-- It seems that snap, the server underlying threepenny gui, does not use the logger when it firsts boots up.
-- All the later messages do seem to use it. Looking into the source of threepenny gui seems that threepenny gui does initialize the logger of snap correctly.

-- The best thing would probably be to not use say in any process that starts before startGUI.

-- [[file:~/projecten/PKI/src/Main.org::*GUI][GUI:1]]
logProcess :: Node.LocalNode -> BSI.ByteString -> IO ()
logProcess node bs = Node.runProcess node (say ("say: " ++ show bs))

main :: IO ()
main = do
  backend <- initializeBackend "localhost" "8030" Node.initRemoteTable
  node <- newLocalNode backend
  let config = defaultConfig {jsStatic = Just ".", jsPort = Just 8200, jsLog = logProcess node} 
  void $ Node.runProcess node $ do
             say "starting db"
             dbpid <- spawnLocal db
             register "db" dbpid
             say "starting startGUI"
             liftIO $ startGUI config (setup node)

setup :: Node.LocalNode -> Window -> UI ()
setup nid w = void $ do
  return w # set title "test neo4j"
  sigmael <- createSigma
  getBody w #+ [element sigmael, controlView nid]
-- GUI:1 ends here

-- input
-- De GUI output is vrijwel hetzelfde voor het invoeren van een nieuwe node voor alleenstaand, child, parent en relatie.
-- De tekst verschilt een beetje maar het is vooral de actie die verschilt.

-- Ervoor gekozen dat het submitten het event is, wel sturen we de text mee. 

-- Voorheen was het idee om de nog niet submitted input het event te maken zodat autocompletion kan worden gedaan.
-- Maar dat is wellicht meer voor binnen een widget zelf en niet buiten een widget.

-- [[file:~/projecten/PKI/src/Main.org::*input][input:1]]
data TextForm = TF { elemTF :: Element, userTF :: Tidings T.Text} 

submitted :: TextForm -> Tidings T.Text
submitted = userTF 

instance Widget TextForm where
  getElement = elemTF 

--    let q = \a -> B.query ("CREATE (n: Concept{longtitle: \"" <> (T.pack a) <> "\"} )")
--   on ((binput <@) . click) b (\a -> liftIO $ B.run p (q a))

-- classid, text, functie
textform :: T.Text -> T.Text -> UI TextForm
textform classid msg = mdo
  g <- GUT.div # set id_ (T.unpack classid)
  b <- button # set text "Add"

  uinput <- entry binput
  let einput = rumors $ userText uinput
  binput <- stepper "" (head <$> unions [einput , "" <$ click b])

  element g #+ [string (T.unpack msg), element uinput,element b] 
  return $ TF g (tidings (T.pack <$> binput) (T.pack <$> einput))
-- input:1 ends here

-- sigmajs


-- [[file:~/projecten/PKI/src/Main.org::*sigmajs][sigmajs:1]]
createSigma :: UI Element
createSigma = do
  g <- GUT.div # set id_ "scontainer"
  graph <- GUT.div # set id_ "sg" # set style [("margin", "auto")]
  s <- string "Hello world"
  b <- button # set text "Appear"

  let js = BS.unpack $ encode testSG
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
