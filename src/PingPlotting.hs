module PingPlotting where

import PingAPI
import PingTypes
import GraphTypes
import GraphPlotter
import GraphBuild
import InternalGraph

partialPlot :: Graph TimeStamp Integer -> IO ()
partialPlot g = do
                    clear
                    graphPrint id fromIntegral g

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