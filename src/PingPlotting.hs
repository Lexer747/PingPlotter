module PingPlotting where

import PingAPI
import PingTypes
import PingGraph
import GraphTypes
import GraphPlotter
import GraphBuild
import InternalGraph

import Control.Monad.State.Strict

type GraphState = State (IO (Graph TimeStamp Integer)) (IO ())

startCycle :: String -> IO ()
startCycle host = plottingCycle $ getInitGraph host

plottingCycle :: IO (Graph TimeStamp Integer) -> IO ()
plottingCycle state = do
    let (printPlot, newState) = runState graphState state
    printPlot
    plottingCycle newState

graphState :: GraphState
graphState = do
        graph <- get
        put $ addPing graph
        return (do
                    g <- graph
                    partialPlot g)

partialPlot :: Graph TimeStamp Integer -> IO ()
partialPlot g = do
    clear
    graphPrint id fromIntegral g