module PingPlotting where

import PingAPI
import PingTypes
import GraphTypes
import GraphPlotter
import GraphBuild

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
            
            
