module Main where

import InternalGraph
import GraphPlotter
import PingTest
import GraphBuild

import Control.Monad
import System.Environment

main = putStrLn "no"
{-
main :: IO ()
main = do
            x <- getArgs
            if length x /= 1 
                then putStrLn "Invalid arg syntax"
                else startCycle $ x !! 0


startCycle :: String -> IO ()
startCycle s = testCycle s $ initPingGraph s

testCycle :: String -> Graph Int Int -> IO ()
testCycle s g = do
                    --clear
                    g' <- testAndDraw s $ return g
                    testCycle s g'

testAndDraw :: String -> IO (Graph Int Int) -> IO (Graph Int Int)
testAndDraw s g = do
                        plot g
                        testAndUpdate s g

plot :: IO (Graph Int Int) -> IO ()
plot g = do
            g' <- g
            drawGraph g'

                    
testAndUpdate :: String -> IO (Graph Int Int) -> IO (Graph Int Int)
testAndUpdate s g = do
                        ping <- pingInt s
                        g' <- g
                        return $ addPingGraph ping g'

maxSetSize :: Int
maxSetSize = 50

initPingGraph :: String -> Graph Int Int
initPingGraph s = Graph {
        maxX = 1,
        minX = 0,
        maxY = 1,
        minY = 0,
        title = s,
        dataSet = []
    }

addPingGraph :: Int -> Graph Int Int -> Graph Int Int
addPingGraph ping g = Graph {
        maxX = newX,
        minX = minX g,
        maxY = newY,
        minY = minY g,
        title = title g,
        dataSet = newSet
    }
    where newX = maxX g + 1
          newY = if ping > (maxY g) then ping else (maxY g)
          coord = [(newX,ping)]
          newSet = if (length s) > maxSetSize then (drop 1 s) ++ coord else s ++ coord
          s = dataSet g
-}