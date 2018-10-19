module PingPlotting where

import PingAPI
import PingTypes
import GraphTypes
import GraphPlotter
import GraphBuild

partialPlot :: Graph TimeStamp Integer -> IO ()
partialPlot = graphPrint id fromIntegral

start :: String -> IO ()
start host = pingCycle $ addPing $ addPing $ addPing $ addPing $ getInitGraph host

pingCycle :: IO (Graph TimeStamp Integer) -> IO ()
pingCycle graph = do
                        g <- graph
                        partialPlot g