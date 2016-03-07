module D3Ex.GraphData where

import Prelude (class Ord, class Eq, class Show, bind, map)
import Data.Array ((!!))
import Data.Int (toNumber)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (readGeneric, defaultOptions, Options)


type GraphData = {
    nodes :: Array Node
  , links :: Array Link
}
type Node = { x :: Number, y :: Number }
type Link = { source :: Node, target :: Node }

convertForD3 :: GraphDataRaw -> Maybe GraphData
convertForD3 (GraphDataRaw d3Data) = do
  let d3nodes = map convertNode d3Data.nodes
  let d3links = traverse (convertLink d3nodes) d3Data.links
  case d3links of
    Nothing -> Nothing
    (Just ls) -> Just { nodes: d3nodes, links: ls }


convertNode :: RawNode -> Node
convertNode (RawNode n) = { x: (toNumber n.x), y: (toNumber n.y) }

convertLink :: Array Node -> RawLink -> Maybe Link
convertLink nodes (RawLink { source: s, target: t }) = do
  sn <- (nodes !! s)
  tn <- (nodes !! t)
  Just { source: sn, target: tn }


-- | all the generic derivations below this line
-- | the newtypes here have generic derivations to read the raw data from JSON
-- | however, for the D3 Force Layout to work the indices in each Link must be
-- | replaced with javascript object references, this is done by converting from
-- | these Generic types to the expected D3 types above)
newtype GraphDataRaw = GraphDataRaw {
    nodes :: Array RawNode
  , links :: Array RawLink
}
newtype RawNode = RawNode { x :: Int, y :: Int }
newtype RawLink = RawLink { source :: Int, target :: Int }

derive instance genericRawNode :: Generic RawNode

myOptions :: Options
myOptions = defaultOptions { unwrapNewtypes = true }

instance showRawNode :: Show RawNode where
  show = gShow
instance eqRawNode :: Eq RawNode where
  eq = gEq
instance ordRawNode :: Ord RawNode where
  compare = gCompare
instance isForeignRawNode :: IsForeign RawNode where
  read = readGeneric myOptions

derive instance genericRawLink :: Generic RawLink

instance showRawLink :: Show RawLink where
  show = gShow
instance eqRawLink :: Eq RawLink where
  eq = gEq
instance ordRawLink :: Ord RawLink where
  compare = gCompare
instance isForeignRawLink :: IsForeign RawLink where
  read = readGeneric myOptions

derive instance genericGraphDataRaw :: Generic GraphDataRaw

instance showGraphDataRaw :: Show GraphDataRaw where
  show = gShow
instance eqGraphDataRaw :: Eq GraphDataRaw where
  eq = gEq
instance ordGraphDataRaw :: Ord GraphDataRaw where
  compare = gCompare
instance isForeignGraphDataRaw :: IsForeign GraphDataRaw where
  read = readGeneric myOptions
