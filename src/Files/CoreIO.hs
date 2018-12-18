module Files.CoreIO where

import Graph.Types
import Graph.Plotter
import Graph.Internal
import Ping.Types
import Ping.Graph
import Ping.API
import Files.Serialization

import System.IO
import Control.Concurrent
import Data.Bits (shiftR)

graphPrint :: (Show a, Show b, IOShow a, IOShow b, RealFrac x, Enum x, Ord x) =>
    (a -> x) -> (b -> x) -> Graph a b -> IO ()
graphPrint cX cY g = do
    maybePlot <- graphToPlot cX cY g
    case maybePlot of
        Nothing -> buffer "graphPrint Failed - cause: window size probably failed"
        Just plot -> buffer $ (show $ length $ plotToPrintString plot) ++ " - " ++plotToPrintString plot

drawGraph :: Graph TimeStamp Integer -> IO ()
drawGraph g = do
    graphPrint id fromIntegral g

findBuffer :: Int -> Int
findBuffer b | b < 16   = 4
findBuffer b            = 1 +  (findBuffer $ b `shiftR` 1)

--fullscreen ~15000 chars (62 * 236)
--half screen ~9000 chars (62 * 138)
setBuffer :: Int -> IO ()
setBuffer i = hSetBuffering stdout (BlockBuffering $ Just i)

adjustBuffer :: String -> IO ()
adjustBuffer s = let size = (length s) * 8 in
                 do
                    bufferMode <- hGetBuffering stdout
                    case bufferMode of
                        BlockBuffering (Just x) | x < size -> setBuffer $ 2 ^ (findBuffer size)
                        _                                  -> return ()

buffer :: String -> IO ()
buffer s = do
            adjustBuffer s
            putStr s
            hFlush stdout

mainLoop :: String -> IO ()
mainLoop host = do
                    innerLoop $ getInitGraph host

innerLoop :: IO (Graph TimeStamp Integer) -> IO ()
innerLoop graph = do
                    g <- addPing graph
                    saveGraph g
                    newG <- readPingGraph (saveLocation g)
                    threadDelay 50
                    drawGraph newG
                    innerLoop $ return newG

singleLoop :: String -> IO ()
singleLoop host = do
                    g <- addPing $ addPing $ addPing $ getInitGraph host
                    saveGraph g
                    newG <- readPingGraph (saveLocation g)
                    threadDelay 50
                    drawGraph newG
                    maybeG <- toInternal id fromIntegral newG
                    case maybeG of
                        Just gg -> buffer $ (show $ window gg)
                        Nothing -> return ()

mainInstance :: String -> IO ()
mainInstance file = do
                        g <- readPingGraph file
                        drawGraph g
 
test = hGetEncoding stdout