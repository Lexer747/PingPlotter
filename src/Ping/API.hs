module Ping.API
    (pingInt, clear)
where
--communicate with underlying OS to get ping values, currently windows specific

import System.Process
import System.Environment
import Text.Regex.Posix
import GHC.IO.Handle

ping :: String -> IO String
ping s = readCreateProcess (proc pingExeStr [s, "-n", "1"]) ""

parsePingString :: IO String -> IO String
parsePingString s = do
    result <- s
    let match = (result =~ groupRegex :: String) =~ countRegex :: String
    return match

groupRegex = "time[=<][0-9]+ms"
countRegex = "[0-9]+"

pingExeStr :: String
pingExeStr = "C:\\Windows\\System32\\PING.EXE"

pingInt :: String -> IO Integer
pingInt s = do 
        n <- p
        return $ read n
    where p = parsePingString $ ping s

clear = system "cls"