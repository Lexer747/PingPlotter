module Files.CoreIO where

import Graph.Types
import Ping.Types
import Ping.Display
import Files.Serialization


mainLoop :: String -> IO ()
mainLoop = undefined

mainInstance :: String -> IO ()
mainInstance file = do
                        g <- readPingGraph file
                        drawGraph g