module Files.CoreIO where

import Graph.Types
import Ping.Types
import Ping.Display
import Ping.Graph
import Files.Serialization


mainLoop :: String -> IO ()
mainLoop host = innerLoop $ getInitGraph host

innerLoop :: IO (Graph TimeStamp Integer) -> IO ()
innerLoop graph = do
                    g <- addPing graph
                    saveGraph g
                    newG <- readPingGraph (saveLocation g)
                    drawGraph newG
                    innerLoop $ return newG

mainInstance :: String -> IO ()
mainInstance file = do
                        g <- readPingGraph file
                        drawGraph g