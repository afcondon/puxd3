module D3Ex where

import Prelude(Unit(),bind,negate)
import Data.Either
import Control.Monad.Eff
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.EasyFFI
import Graphics.D3.Base
import Graphics.D3.Layout.Base
import Graphics.D3.Layout.Force
import Graphics.D3.Request
import Graphics.D3.Scale
import Graphics.D3.Selection
import Graphics.D3.Util
import Data.List (List(Nil), singleton)

type GraphData =
  { nodes :: Array Node
  , links :: Array Link
  }

type Node = { x :: Number, y :: Number }
type Link = { source :: Node, target :: Node }

d3main :: forall eff. Eff (d3 :: D3 | eff) Unit
d3main = do
  let canvasWidth = 960.0
      canvasHeight = 500.0

  force <- forceLayout
    .. size { width: canvasWidth, height: canvasHeight }
    .. charge (-400.0)
    .. linkDistance 40.0

  drag <- force ... drag
    .. onDragStart dragStartHandler

  svg <- rootSelect "body"
    .. append "svg"
    .. attr "width" canvasWidth
    .. attr "height" canvasHeight

  json "http://localhost:8080/graph" \(Right v) -> do
    let graph = toGraphData v

    force
     ... nodes graph.nodes
      .. links graph.links
      .. start

    link <- svg ... selectAll ".link"
        .. bindData graph.links
      .. enter .. append "line"
        .. attr "class" "link"

    node <- svg ... selectAll ".node"
        .. bindData graph.nodes
      .. enter .. append "circle"
        .. attr "class" "node"
        .. attr "r" 12.0
        .. onDoubleClick doubleClickHandler
        .. createDrag drag

    force ... onTick \_ -> do
      link
       ... attr' "x1" (\d -> d.source.x)
        .. attr' "y1" (\d -> d.source.y)
        .. attr' "x2" (\d -> d.target.x)
        .. attr' "y2" (\d -> d.target.y)

      node
       ... attr' "cx" _.x
        .. attr' "cy" _.y

dragStartHandler :: forall d. d -> D3Eff Unit
dragStartHandler = ffi ["d"] "d3.select(this).classed('fixed', d.fixed = true);"

doubleClickHandler :: forall d. d -> D3Eff Unit
doubleClickHandler = ffi ["d"] "d3.select(this).classed('fixed', d.fixed = false);"

toGraphData :: Foreign -> GraphData
toGraphData = ffi ["g"] "g"

ffi = unsafeForeignFunction
