module View where

import Prelude hiding (div)
import DOM (DOM())
import Pux
import Pux.DOM.HTML.Elements (div, p, button, text, span, input)
import Pux.DOM.HTML.Attributes (onClick, send, className, KeyboardEvent, onKeyUp, placeholder)

import State
import Actions

-- |=================================    VIEW      =================================
view :: State -> VirtualDOM
view (State state) = div ! className "controls" $ do
  p $ text (show state.counter)
  p $ text (show state.banner)
  p ! className "btn-group" $ do
    button ! onClick (send ButtonOne)   <> className "btn btn-primary" $ text "ButtonOne"
    button ! onClick (send ButtonTwo)   <> className "btn btn-info"    $ text "ButtonTwo"
    button ! onClick (send ButtonThree) <> className "btn btn-warning" $ text "ButtonThree"
  span $ text " "
  p ! className "btn-group" $ do
    button ! onClick (send ButtonFour) <> className "btn btn-xs btn-info" $ text "Socket"
