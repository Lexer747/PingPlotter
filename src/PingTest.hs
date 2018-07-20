module PingTest where

import System.Process
import GHC.IO.Handle
import Text.Regex


ping :: String -> IO String
ping s = readCreateProcess (proc ("ping \"" ++ s ++ "\" -n 1") []) ""

parsePingString :: IO String -> IO String
parsePingString s = do
                        result <- s
                        putStrLn $ "parsePingString: \"" ++ result ++ "\""
                        return $ case (matchRegexAll pingRegex result) of
                                    Nothing -> "0"
                                    Just (_,time,_,_) -> reverse $ drop 2 $ reverse $ drop 5 time
                                    
pingRegex :: Regex
pingRegex = mkRegex "time[=<][0-9]+ms"


pingInt :: String -> IO Int
pingInt s = do 
                n <- p
                return $ read n
    where p = parsePingString $ ping s
    
    
clear = system "cls"

          