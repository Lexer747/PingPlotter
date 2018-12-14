module Ping.Display where
--actually draw a ping graph to the window

import Ping.API (clear)
import Ping.Types
import Graph.Types
import Graph.Plotter (graphPrint)

drawGraph :: Graph TimeStamp Integer -> IO ()
drawGraph g = do
    clear
    graphPrint id fromIntegral g