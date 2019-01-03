{-# LANGUAGE ScopedTypeVariables #-}

module Main where

--import all our files
import Graph.Internal
import Graph.Build
import Graph.Plotter
import Graph.Types
import Ping.API
import Ping.Graph
import Ping.Types
import Files.Serialization
import Files.CoreIO
import Utils

import Control.Monad
import System.Environment
import System.Directory (doesFileExist)
import Control.Exception

exeName = "Ping-v2-0-0"

main = do
        x <- getArgs
        if length x == 1
            then parseArgs x
            else putStrLn $ " Invalid arguments, expecting \n$> " ++ exeName ++ " \"www.example.com\"\n Or with an IP address:\n$> " ++ exeName ++ " \"128.0.0.1\""

parseArgs :: [String] -> IO ()
parseArgs (hostOrFile:[]) = do
    file <- doesFileExist hostOrFile
    if file
        then catch (mainInstance hostOrFile)
                   (\(e :: AsyncException) -> return ())
        else mainLoop hostOrFile