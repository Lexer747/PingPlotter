module Main where

import Control.Monad
import System.Environment
import System.Console.Terminal.Size

import InternalGraph
import GraphPlotter
import GraphTypes
import PingAPI
import GraphBuild
import PingTypes

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