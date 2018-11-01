module PingPlotting where

import PingAPI
import PingTypes
import GraphTypes
import GraphPlotter
import GraphBuild
import InternalGraph

import Control.Monad.State.Strict

{-


start :: String -> IO ()
start host = pingCycle $ getInitGraph host

pingCycle :: IO (Graph TimeStamp Integer) -> IO ()
pingCycle graph = do 
                    g <- graph
                    partialPlot g
                    pingCycle $! addPing graph --perform a strict evaluation
            
            
getSample :: String -> IO (Graph TimeStamp Integer)
getSample host = cycle' init
    where init = getInitGraph host

cycle' :: IO (Graph TimeStamp Integer) -> IO (Graph TimeStamp Integer)
cycle' graph = cycle' $! addPing $! graph
-}

type GraphState = State (IO (Graph TimeStamp Integer)) (IO ())

startCycle :: String -> IO ()
startCycle host = plottingCycle $ getInitGraph host

plottingCycle :: IO (Graph TimeStamp Integer) -> IO ()
plottingCycle state = do
                    let (io, newState) = runState evalGraphState state
                    io
                    plottingCycle newState

evalGraphState :: GraphState
evalGraphState = do
    graph <- get
    put $ addPing graph
    return (do
                g <- graph
                partialPlot g)

    
partialPlot :: Graph TimeStamp Integer -> IO ()
partialPlot g = do
    clear
    graphPrint id fromIntegral g