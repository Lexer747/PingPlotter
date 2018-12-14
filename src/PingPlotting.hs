module PingPlotting where

import PingAPI
import PingTypes
import PingGraph
import GraphTypes
import GraphPlotter
import GraphBuild
import InternalGraph

import Control.Monad.State.Strict

partialPlot :: Graph TimeStamp Integer -> IO ()
partialPlot g = do
    clear
    graphPrint id fromIntegral g