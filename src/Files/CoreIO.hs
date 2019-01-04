module Files.CoreIO where

import Graph.Types
import Graph.Plotter
import Graph.Internal
import Graph.Build (chooseGraph)
import Ping.Types
import Ping.Graph
import Ping.API
import Files.Serialization

import System.IO
import Control.Concurrent
import Data.Bits (shiftR)

globalScale :: Double
globalScale = 1.2

--Given two conversion functions and a graph, actually plot the graph to cmd
graphPrint :: (Show a, Show b, IOShow a, IOShow b, RealFrac x, Enum x, Ord x) =>
    (a -> x) -> (b -> x) -> Graph a b -> IO ()
graphPrint cX cY g = do
    maybePlot <- graphToPlot cX cY g --plot the graph
    case maybePlot of
        Nothing -> buffer "graphPrint Failed - cause: window size probably failed"
        Just plot -> buffer $ "\n" ++ (plotToPrintString plot) ++ "\n"
        -- ^ convert the plot to the string and buffer it to stdout

--Given two conversion functions and a graph, plot the tail of the data to the cmd
chooseGraphPrint :: (Show a, Enum a, Ord a, Show b, Enum b, Ord b, IOShow a, IOShow b,
    RealFrac x, Enum x, Ord x) =>
    (a -> x) -> (b -> x) -> Double -> Graph a b -> IO ()
chooseGraphPrint cX cY scale g = do
    maybePlot <- chooseGraphToPlot cX cY scale g --plot the graph
    case maybePlot of
        Nothing -> buffer "graphPrint Failed - cause: window size probably failed"
        Just plot -> buffer $ "\n" ++ (plotToPrintString plot) ++ "\n"
        -- ^ convert the plot to the string and buffer it to stdout

-- Given a Ping graph, draw it to the cmd
drawGraph :: Graph TimeStamp Integer -> IO ()
drawGraph g = do
    graphPrint id fromIntegral g

-- Given a Ping graph, draw most of it to the cmd
chooseDrawGraph :: Double -> Graph TimeStamp Integer -> IO ()
chooseDrawGraph scale g = do
    chooseGraphPrint id fromIntegral scale g

-- Given a number of chars, find the next index power of 2 which is bigger than or equal
-- findBuffer 18          = 32
-- findBuffer (2^4) + ... = 2^5
findBuffer :: Int -> Int
findBuffer b | b < 16   = 4
findBuffer b            = 1 +  (findBuffer $ b `shiftR` 1) -- shift right is a divide by 2

--set the size of the buffer of stdout (in bytes)
setBuffer :: Int -> IO ()
setBuffer i = hSetBuffering stdout (BlockBuffering $ Just i)

--Given a string, compare the length of that string to the buffer size of stdout
--if its bigger than raise the size to the next power of 2
adjustBuffer :: IO ()
adjustBuffer = do
    s <- size
    case s of
        Nothing -> return ()
        Just w  ->  let size = (height w) * (width w) in do
                    bufferMode <- hGetBuffering stdout
                    case bufferMode of
                        BlockBuffering (Just x) | x < size  -> setBuffer $ 2 ^ (findBuffer size)
                        BlockBuffering (Just x) | x >= size -> return () --don't change size
                        _                                   -> setBuffer $ 2 ^ (findBuffer size)

-- adjust putStr, to check for buffering
-- this will buffer the entire string before flushing
buffer :: String -> IO ()
buffer s = do
            adjustBuffer
            putStr s
            hFlush stdout

mainLoopWithNoSave :: String -> IO ()
mainLoopWithNoSave = undefined -- TODO

--repeat capturing a ping forever, but add on to a previous file
mainLoopWithPreserve :: String -> IO ()
mainLoopWithPreserve host = do
                                innerLoop $ readPingGraph $ host ++ ".ping"

--repeat capturing a ping forever
mainLoop :: String -> IO ()
mainLoop host = do
                    innerLoop $ getInitGraph host

--given a ping graph, add another ping point to it
innerLoop :: IO (Graph TimeStamp Integer) -> IO ()
innerLoop graph = do
                    g <- addPing graph --add the ping
                    saveGraph g --save the graph to file (forces evaluation)
                    newG <- readPingGraph (saveLocation g) --read the new graph
                    threadDelay 50
                    chooseDrawGraph globalScale newG --actually draw the graph
                    innerLoop $ return newG --repeat with the new graph


innerLoopNoSave :: IO (Graph TimeStamp Integer) -> IO ()
innerLoopNoSave graph = do
                    g <- addPing graph
                    g' <- chooseGraph globalScale g
                    saveGraph g'
                    newG <- readPingGraph (saveLocation g')
                    threadDelay 50
                    drawGraph newG
                    innerLoopNoSave $ return newG

--given a file name draw the graph inside that file
mainInstance :: String -> IO ()
mainInstance file = do
                        g <- readPingGraph file
                        drawGraph g