module D3Ex where

import Prelude(Unit(),bind,negate)
-- import Data.Either
import Control.Monad.Eff (Eff)
-- import Data.Foreign (Foreign)
import Data.Foreign.EasyFFI(unsafeForeignFunction)
import Graphics.D3.Base (D3Eff, D3)
import Graphics.D3.Layout.Base (links, nodes, size)
import Graphics.D3.Layout.Force (onTick, createDrag, start, onDragStart, drag, linkDistance, charge, forceLayout, ForceLayout)
-- import Graphics.D3.Request
-- import Graphics.D3.Scale
import Graphics.D3.Selection (attr', onDoubleClick, attr, append, enter, bindData, selectAll, rootSelect)
import Graphics.D3.Util ((..), (...))
-- import Data.List (List(Nil), singleton)

import D3Ex.GraphData (GraphData)

d3FL :: GraphData -> forall eff. Eff (d3 :: D3 | eff) ForceLayout
d3FL graph = do
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

singleClickHandler :: forall d. d -> D3Eff Unit
singleClickHandler = ffi ["d"] "d3.select(this).classed('single', d.fixed = true);"

doubleClickHandler :: forall d. d -> D3Eff Unit
doubleClickHandler = ffi ["d"] "d3.select(this).classed('fixed', d.fixed = false);"

-- toGraphData :: Foreign -> GraphData
-- toGraphData = ffi ["g"] "g"

ffi :: forall a. Array String -> String -> a
ffi = unsafeForeignFunction
