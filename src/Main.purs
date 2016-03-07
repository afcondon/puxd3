module Main where

import Prelude (Unit, bind, ($), (-), (+), (++), return, show)
import WebSocket (WEBSOCKET, Connection(Connection), Message(Message), URL(URL), runMessageEvent, runMessage, newWebSocket)
import Control.Monad.Aff (later', launchAff)
import Network.HTTP.Affjax as A
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Var (($=))
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foreign (F, ForeignError)
import Data.Foreign.Class (readJSON)
import Data.List (singleton)
import DOM (DOM())
import Pux (Update, app) as Pux
import Pux.Render.DOM (renderToDOM) as Pux
import Signal (Signal) as S
import Signal.Channel (CHANNEL(), Channel, channel, send, subscribe) as S
import Graphics.D3.Base (D3, D3Eff)

import State (State(State))
import View (view)
import Actions (Action(ReceiveWSData, ButtonFour, ButtonThree, ReceiveAJAXData, ButtonTwo, ButtonOne))
import D3Ex (d3FL)
import D3Ex.GraphData (GraphDataRaw, convertForD3)

-- |=================================    STATE      =================================
initialState :: S.Channel Action -> String -> forall e. Eff (ws :: WEBSOCKET|e) State
initialState chan url = do
  connection@(Connection ws) <- newWebSocket (URL url) []
  ws.onmessage $= \event -> do
      let received = runMessage (runMessageEvent event)
      log "message received from websocket"
      S.send chan ((ReceiveWSData received) :: Action)
  let state = State { counter: 0, banner: "initial string", socket: connection }
  return state

-- |=================================    UPDATE      =================================
update :: forall eff. Pux.Update
          (ajax :: A.AJAX
          , err :: EXCEPTION
          , console :: CONSOLE
          , d3  :: D3
          , ws :: WEBSOCKET | eff)
          State
          Action
update action (State state) input =
  case action of
    ButtonOne ->
      { state: State state { counter = state.counter + 1 }
      , effects: [ do log "set view to ButtonOne" ] }
    ButtonTwo ->
      { state: State state { counter = state.counter - 1 }
      , effects: [ do log "set view to ButtonTwo" ] }
    ReceiveWSData msg ->
      { state: State state { banner = msg }
      , effects: []
      }
    ReceiveAJAXData v ->
      { state: State state { banner = "fetched JSON using AJAX" }
      , effects: [ case convertForD3 v of
                   Nothing      -> log "Error converting data for D3"
                   (Just graph) -> do
                                    d3FL graph puxD3ClickHandler
                                    log "successfully ran the D3 ForceLayout"
                 ]
      }
    ButtonThree ->
      { state: State state { banner = "Kicking it to D3 in 3...2...1..." }
      , effects: [ doAjaxCall ]
      }
    ButtonFour ->
      { state: State state
      , effects: [ do doWebSocketCall state.socket ]
    }
  where
    -- don't know how to write signature for this function!
    doAjaxCall = launchAff $ later' 1500 $ do
      res <- A.get "http://localhost:8080/graph"  -- requires json-server or similar running on port 8080
      let response = readJSON res.response :: F GraphDataRaw
      liftEff $ case response of
          (Left err) -> log "Error parsing JSON!" -- ++ (show err)
          (Right gd) -> S.send input (singleton (ReceiveAJAXData gd))
    doWebSocketCall :: forall e. Connection -> Eff (ws::WEBSOCKET|e) Unit
    doWebSocketCall (Connection ws) =  do ws.send(Message "button four sends this message")

puxD3ClickHandler :: forall d e. S.Channel Action -> d  -> Eff ( channel :: S.CHANNEL | e ) Unit
puxD3ClickHandler chan d = S.send chan ButtonOne


-- |=================================     MAIN      =================================
main :: forall e. Eff ( ws::WEBSOCKET
                      , channel::S.CHANNEL
                      , dom::DOM
                      , ajax::A.AJAX
                      , err::EXCEPTION
                      , console::CONSOLE
                      , d3 :: D3 | e ) Unit
main = do
  -- d3main
  wsInput <- S.channel (ReceiveWSData "foo")
  d3Input <- S.channel ButtonOne
  appState <- initialState wsInput "ws://echo.websocket.org" -- forall e. Eff (ws :: WEBSOCKET|e) State
  let wsSignal = S.subscribe wsInput :: S.Signal Action
  let d3Signal = S.subscribe d3Input :: S.Signal Action
  Pux.renderToDOM "#app" =<< Pux.app
    { state: appState
    , update: update
    , view: view
    , inputs: [wsSignal, d3Signal]
    }
