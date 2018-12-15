module Files.CoreIO where

import Graph.Types
import Graph.Plotter
import Ping.Types
import Ping.Graph
import Ping.API
import Files.Serialization

import System.IO
import Control.Concurrent

graphPrint :: (Show a, Show b, IOShow a, IOShow b, RealFrac x, Enum x, Ord x) =>
    (a -> x) -> (b -> x) -> Graph a b -> IO ()
graphPrint cX cY g = do
    maybePlot <- graphToPlot cX cY g
    case maybePlot of
        Nothing -> buffer "graphPrint Failed - cause: window size probably failed"
        Just plot -> buffer $ plotToPrintString plot

drawGraph :: Graph TimeStamp Integer -> IO ()
drawGraph g = do
    graphPrint id fromIntegral g

bufferSize :: Int
bufferSize = 2 ^ 18

editStdout :: IO ()
editStdout = hSetBuffering stdout (BlockBuffering $ Just bufferSize)

buffer :: String -> IO ()
buffer s = do
            putStr s
            hFlush stdout

mainLoop :: String -> IO ()
mainLoop host = do
                    editStdout
                    innerLoop $ getInitGraph host

innerLoop :: IO (Graph TimeStamp Integer) -> IO ()
innerLoop graph = do
                    g <- addPing graph
                    saveGraph g
                    newG <- readPingGraph (saveLocation g)
                    threadDelay 50
                    --clear
                    drawGraph newG
                    innerLoop $ return newG

mainInstance :: String -> IO ()
mainInstance file = do
                        g <- readPingGraph file
                        drawGraph g