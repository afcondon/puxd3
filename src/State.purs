module State where

import WebSocket

-- |=================================    STATE      =================================
data State = State { counter :: Int, banner :: String, socket :: Connection }
