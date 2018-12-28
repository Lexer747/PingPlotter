module Ping.API
    (pingInt, clear)
where
--communicate with underlying OS to get ping values, currently very windows specific

import System.Process
import System.Environment
import Text.Regex.Posix
import GHC.IO.Handle

--literally call the windows ping CLI program
ping :: String -> IO String
ping s = readCreateProcess (proc pingExeStr [s, "-n", "1"]) ""

--use a regex to get out the value of the ping
--TODO handle no found ping value
parsePingString :: IO String -> IO String
parsePingString s = do
    result <- s
    let match = (result =~ groupRegex :: String) =~ countRegex :: String
    return match

groupRegex = "time[=<][0-9]+ms"
countRegex = "[0-9]+"

pingExeStr :: String
pingExeStr = "C:\\Windows\\System32\\PING.EXE"

--Combine all the functions
--given a host return the IO wrapped ping value
pingInt :: String -> IO Integer
pingInt s = do 
        n <- p
        return $ read n
    where p = parsePingString $ ping s

clear = system "cls"