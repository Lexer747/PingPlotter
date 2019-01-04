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
import System.Directory (doesFileExist, removePathForcibly)
import Control.Exception

---------------------------------- constants ----------------------------------

exeName = "Ping-v2-0-0"
cmdMaxWidth :: Int
cmdMaxWidth = 70

--------------------------------------------------------------------------------

--keep the main simple
main = do
        x <- getArgs
        parseArgs x

----------------------------- parsing arguments ----------------------------------

--Taking the arguments from command line, process them based on the number of them
parseArgs :: [String] -> IO ()

--Exactly 2 arguments:
--  Try to parse the first argument as an option
parseArgs (option:hostOrFile:[]) = case (parseOption option) of
    Nothing          -> parseError --the first arg wasn't a valid option
    Just ((_,_,f),_) -> handleArgs f hostOrFile --pass the option and the next arg to the next step

--Exactly 1 argument:
--  Try to parse the first argument as an option, in the case of '-h'
parseArgs (hostOrFile:[])        = case (parseOption hostOrFile) of
    Nothing          -> handleArgs Default hostOrFile --no option so pass to next step
    Just _           -> parseError --ignore what option was parsed, just show help anyway

--Exactly 0 arguments:
parseArgs []                     = parseError

--Given a string attempt to match it to one of the options
parseOption :: String -> Maybe Option
parseOption ('-':'-':long)  = case (filter (\((_,l,_),_) -> l == long) options) of
                                [] -> Nothing --nothing matched
                                (x:[]) -> Just x --only one option should match, return it
                                _      -> Nothing --more than option matched
parseOption ('-':short)     = case (filter (\((s,_,_),_) -> s == short) options) of
                                [] -> Nothing
                                (x:[]) -> Just x
                                _      -> Nothing
parseOption _               = Nothing

--Shorter name for when the user does something wrong
parseError = putStr helpString

-----------------------------------------------------------------------------------

--Given a flag, based on the option, and a user input (hostOrFile)
--handle it
handleArgs :: Flag -> String -> IO ()

--NoOptions, handle the user input using mainLoop to process it
handleArgs Default hostOrFile  = handleFile (cntrlC . mainLoop) hostOrFile

--Preserve, so we check if there is something to preserve
handleArgs Preserve hostOrFile = do
    file <- doesFileExist $ hostOrFile ++ ".ping"
    if file
        then handleFile (cntrlC . mainLoopWithPreserve) hostOrFile --something to preserve
        else handleFile (cntrlC . mainLoop) hostOrFile --nothing to preserve

--NoSave, so we need a different interrupt handler, and different loop
handleArgs NoSave hostOrFile   = handleFile mainLoopWithNoSaveFinally hostOrFile
    where mainLoopWithNoSaveFinally xs = catch (mainLoopWithNoSave xs)
                                               (\(e :: AsyncException) -> removePathForcibly $ hostOrFile ++ ".ping")

handleArgs Help _              = parseError

--given a way to process a host or file, check if the user directly requested a single file first
--otherwise perform the action on the host
handleFile :: (String -> IO ()) -> String -> IO ()
handleFile action hostOrFile = do
    file <- doesFileExist hostOrFile
    if file
        then mainInstance hostOrFile --we have a file
        else action hostOrFile

--The usage String, documents all the options, and general usage
helpString :: String
helpString = "Usage:\n\n\
    \    " ++ exeName ++ " [options] [File|IP address|Website]\n\n\
    \Options:\n" ++ concatMap (printOption "    " cmdMaxWidth) options

-- A list off all the options, hard coded to manually change
options :: [Option]
options = [optionPreserve, optionHelp, optionNoSave]

optionPreserve :: Option
optionPreserve = (("p","preserve", Preserve),"If a graph already exists, then add to it. Without this argument the old graph is overwritten")

optionHelp :: Option
optionHelp = (("h","help", Help), "Show this message")

optionNoSave :: Option
optionNoSave = (("n","nosave", NoSave), "Will not save a '.ping' file after running, *note* a .ping file is used during execution, hence this will overwrite any current '.ping' file with the same name")

---------------------------------------------------------------------------
--Types and functions for dealing with command line options

            --((small flag, big flag, haskell option), usage )
type Option = ((String    , String  , Flag          ), String)

--A haskell type to handle express every option
data Flag = Help
          | Preserve
          | NoSave
          | Default
    deriving (Show,Eq)

--given an starting indent, a max command line size, and an option.
--Create the string to explain and document the option
printOption :: String -> Int -> Option -> String
printOption indent maxWidth ((small,large,_),usage) = lead ++ (wrap init maxWidth usage)
    where lead = indent ++ "-" ++ small ++ " --" ++ large ++ ": "
          init = length lead

--Given an indent, a max width, and a string, reformat the string to wrap every time it exceeds
--the max width, with the indent on each new line.
--This also doesn't indent the initial line
wrap :: Int -> Int -> String -> String
wrap indent maxWidth _ | indent > maxWidth = error "wrap passed invalid indent and width sizes [" ++ (show indent) ++ "] > [" ++ (show maxWidth) ++ "]"
wrap indent maxWidth toWrap = drop indent $ wrap_ indent maxWidth toWrap

--same as wrap, but does not drop the original indent
wrap_ :: Int -> Int -> String -> String
wrap_ _      _        []      = []
wrap_ indent maxWidth toWrap  = spaces ++ take (maxWidth - indent) toWrap ++ "\n" ++ (wrap_ indent maxWidth $ drop (maxWidth - indent) toWrap)
    where spaces = replicate indent ' '

----------------------------------------------------------------------------

--A default handling of user interrupts
cntrlC :: IO () -> IO ()
cntrlC action = catch (action)
                      (\(e :: AsyncException) -> return ())