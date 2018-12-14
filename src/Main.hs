module Main where

import Control.Monad
import System.Environment
import System.Console.Terminal.Size

import Graph.Internal
import Graph.Build
import Graph.Plotter
import Graph.Types
import Ping.API
import Ping.Graph
import Ping.Plotting
import Ping.Types
import Utils

main = putStrLn "Test"
{-
main :: IO ()
main = do
            x <- getArgs
            if length x /= 1 
                then putStrLn "Invalid arg syntax"
                else let host = head x in
                        do 
                            initPing <- pingInt host
                            start host initPing
                            -}