{-# LANGUAGE ScopedTypeVariables #-} --for exceptions
{-# LANGUAGE CPP #-} --for windows/linux differntation

module Ping.API
    (pingInt
    , clear
    ) where
--communicate with underlying OS to get ping values, currently very windows specific

import System.Process
--import System.Environment
import Text.Regex.Posix
--import GHC.IO.Handle
import Control.Exception --for catching errors

--literally call the windows ping CLI program
--try catch the program
ping :: String -> IO String
ping s = catch (readCreateProcess (proc pingExeStr [s, pingExeOption, "1"]) "")
               (\(_ :: IOException) -> do --should log error
                                        return "") --empty string on ping.exe error

--use a regex to get out the value of the ping
parsePingString :: IO String -> IO String
parsePingString s = do
    result <- s
    let matched = (result =~ groupRegex :: String) =~ countRegex :: String
    return matched


countRegex :: String
countRegex = "[0-9]+"

pingExeStr :: String
pingExeOption :: String
groupRegex :: String

#ifdef mingw32_HOST_OS
pingExeStr = "C:\\Windows\\System32\\PING.EXE"
pingExeOption = "-n"
groupRegex = "time[=<][0-9]+ms"

#else

pingExeStr = "/bin/ping"
pingExeOption = "-c"
groupRegex = "time=[0-9]+.[0-9]+ ms"
#endif

--Combine all the functions
--given a host return the IO wrapped ping value
-- OR nothing if the ping failed for some reason
pingInt :: String -> IO (Maybe Integer)
pingInt s = do
        n <- p
        case n of
            [] -> return $ Nothing --regex matched nothing, or error
            _  -> return $ Just $ read n
    where p = parsePingString $ ping s

clear = system "cls"
