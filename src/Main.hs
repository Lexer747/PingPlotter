module Main where

import Control.Monad
import System.Environment
import System.Console.Terminal.Size
import Data.Dates

import InternalGraph
import GraphPlotter
import GraphTypes
import PingTest
import GraphBuild
import Ping



main :: IO ()
main = do
            x <- getArgs
            if length x /= 1 
                then putStrLn "Invalid arg syntax"
                else let host = head x in
                        do 
                            initPing <- pingInt host
                            start host initPing
                    
initPingGraph :: String -> Integer -> Graph Integer Integer
initPingGraph host init = namedListToGraph [(1,init)] host ("Ping #", "time ms")


start :: String -> Integer -> IO ()
start host init = pingCycle host $ initPingGraph host init

pingCycle :: String -> Graph Integer Integer -> IO ()
pingCycle host graph = undefined