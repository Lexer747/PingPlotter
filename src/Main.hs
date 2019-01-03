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
cmdMaxWidth :: Int
cmdMaxWidth = 70

main = do
        x <- getArgs
        parseArgs x

parseArgs :: [String] -> IO ()
parseArgs (option:hostOrFile:[]) = case (parseOption option) of
        Nothing          -> parseError
        Just ((_,_,f),_) -> handleArgs f hostOrFile
parseArgs (hostOrFile:[]) = case (parseOption hostOrFile) of
        Nothing          -> handleArgs Default hostOrFile
        Just _           -> parseError
parseArgs []              = parseError

handleArgs :: Flag -> String -> IO ()
handleArgs Default hostOrFile  = handleFile mainLoop hostOrFile
handleArgs Preserve hostOrFile = do
    file <- doesFileExist $ hostOrFile ++ ".ping"
    if file
        then handleFile mainLoopWithPreserve hostOrFile
        else handleFile mainLoop hostOrFile
handleArgs Help _              = parseError

handleFile :: (String -> IO ()) -> String -> IO ()
handleFile action hostOrFile = do
    file <- doesFileExist hostOrFile
    if file
        then mainInstance hostOrFile
        else catch (action hostOrFile)
                   (\(e :: AsyncException) -> return ())

parseOption :: String -> Maybe Option
parseOption ('-':'-':long)  = case (filter (\((_,l,_),_) -> l == long) options) of
                                [] -> Nothing
                                (x:[]) -> Just x
                                _      -> Nothing
parseOption ('-':short)     = case (filter (\((s,_,_),_) -> s == short) options) of
                                [] -> Nothing
                                (x:[]) -> Just x
                                _      -> Nothing
parseOption _               = Nothing

parseError = putStr helpString

helpString = "Usage:\n\n\
    \    " ++ exeName ++ " [options] [File|IP address|Website]\n\n\
    \Options:\n" ++ concatMap (printOption "    " cmdMaxWidth) options

options :: [Option]
options = [optionPreserve, optionHelp, optionNoSave]

optionPreserve :: Option
optionPreserve = (("p","preserve", Preserve),"If a graph already exists, then add to it. Without this argument the old graph is overwritten")

optionHelp :: Option
optionHelp = (("h","help", Help), "Show this message")

optionNoSave :: Option
optionNoSave = (("n","nosave", NoSave), "Will not save a '.ping' file after running, *note* a .ping file is used during execution")

-----------------------------------------------------------------------

            --((small flag, big flag, haskell option), usage )
type Option = ((String    , String  , Flag          ), String)

data Flag = Help
          | Preserve
          | NoSave
          | Default
    deriving (Show,Eq)

printOption :: String -> Int -> Option -> String
printOption indent maxWidth ((small,large,_),usage) = lead ++ (wrap init maxWidth usage)
    where lead = indent ++ "-" ++ small ++ " --" ++ large ++ ": "
          init = length lead

wrap :: Int -> Int -> String -> String
wrap indent maxWidth toWrap = drop indent $ wrap_ indent maxWidth toWrap

wrap_ :: Int -> Int -> String -> String
wrap_ _      _        []      = []
wrap_ indent maxWidth toWrap  = spaces ++ take (maxWidth - indent) toWrap ++ "\n" ++ (wrap_ indent maxWidth $ drop (maxWidth - indent) toWrap)
    where spaces = replicate indent ' '