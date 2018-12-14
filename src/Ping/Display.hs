module Ping.Display where
--actually draw the graph to the window

import Ping.API
import Ping.Types
import Ping.Graph
import Graph.Types
import Graph.Plotter
import Graph.Build
import Graph.Internal

import Control.Monad.State.Strict

partialPlot :: Graph TimeStamp Integer -> IO ()
partialPlot g = do
    clear
    graphPrint id fromIntegral g