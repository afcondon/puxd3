module Main where

import Prelude (class Show, Unit, bind, (++), ($), (-), (+), return)
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
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readJSON, readProp)
import Data.List (singleton)
import DOM (DOM())
import Pux (Update, app) as Pux
import Pux.Render.DOM (renderToDOM) as Pux
import Signal (Signal) as S
import Signal.Channel (CHANNEL(), Channel, channel, send, subscribe) as S

import State (State(State))
import View (view)
import Actions (Action(ReceiveWSData, ButtonFour, ButtonThree, ReceiveAJAXData, ButtonTwo, ButtonOne))

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

-- |=================================    Ajax      =================================
data AjaxMsg = AjaxMsg { version :: String, language :: String } -- {"version":"4.2.10092","language":"javax"}

instance showAjaxMsg :: Show AjaxMsg where
  show (AjaxMsg m) = "{ \"version\": \"" ++ m.version ++ "\"language\": \"" ++ m.language ++ "\" }"

instance ajaxMessageIsForeign :: IsForeign AjaxMsg where
  read value = do
    version  <- readProp "version" value
    language <- readProp "language" value
    return $ AjaxMsg { version: version, language: language }

-- |=================================    UPDATE      =================================
update :: forall eff. Pux.Update
          (ajax :: A.AJAX, err :: EXCEPTION, console :: CONSOLE, ws :: WEBSOCKET | eff)
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
    ReceiveAJAXData msg ->
      { state: State state { banner = msg }
      , effects: [ do log $ "Updated new state: " ++ msg ]
      }
    ButtonThree ->
      { state: State state { banner = "Loading data from server..." }
      , effects: [ doAjaxCall ]
      }
    ButtonFour ->
      { state: State state
      , effects: [ do doWebSocketCall state.socket ]
    }
  where
    -- don't know how to write signature for this function!
    doAjaxCall = launchAff $ later' 1500 $ do
      res <- A.get "http://localhost:8080/version"  -- requires something like json-server running on port 8080
      let response = readJSON res.response :: F AjaxMsg
      liftEff $ case response of
          (Left err) -> log "Error parsing JSON!"
          (Right (AjaxMsg msg)) -> S.send input (singleton (ReceiveAJAXData msg.version))
    doWebSocketCall :: forall e. Connection -> Eff (ws::WEBSOCKET|e) Unit
    doWebSocketCall (Connection ws) =  do ws.send(Message "button four sends this message")

-- |=================================    MAIN      =================================
main :: forall e. Eff ( ws::WEBSOCKET
                      , channel::S.CHANNEL
                      , dom::DOM
                      , ajax::A.AJAX
                      , err::EXCEPTION
                      , console::CONSOLE | e ) Unit
main = do
  wsInput <- S.channel (ReceiveWSData "foo")
  appState <- initialState wsInput "ws://echo.websocket.org" -- forall e. Eff (ws :: WEBSOCKET|e) State
  let wsSignal = S.subscribe wsInput :: S.Signal Action
  Pux.renderToDOM "#app" =<< Pux.app
    { state: appState
    , update: update
    , view: view
    , inputs: [wsSignal]
    }
