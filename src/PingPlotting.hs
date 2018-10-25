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
            
            
getSample :: String -> IO()
getSample host = do
                    let g1 = addPing $ getInitGraph host 
                    g1' <- g1
                    graphPrint id fromIntegral g1'
                    let g2 = addPing g1
                    g2' <- g2
                    graphPrint id fromIntegral g2'
                    let g3 = addPing g2
                    g3' <- g3
                    graphPrint id fromIntegral g3'
                    let g4 = addPing g3
                    g4' <- g4
                    graphPrint id fromIntegral g4'
                    let g5 = addPing g4
                    g5' <- g5
                    graphPrint id fromIntegral g5'
                    i <- toInternal id fromIntegral g5'
                    case i of 
                        Just x -> putStr $ show x
                        Nothing -> putStr "ops"
                    
