module Actions where

import D3Ex.GraphData (GraphDataRaw)

-- |=================================    ACTIONS      =================================
data Action = ButtonOne | ButtonTwo | ButtonThree | ReceiveAJAXData GraphDataRaw | ButtonFour | ReceiveWSData String
