module Ping.Graph where
-- Build a graph containing ping values

import Ping.Types
import Ping.API
import Graph.Types
import Graph.Build (namedListToGraph, addPoint)

pingRetry :: Int
pingRetry = 10

--given two starting pings create a graph with all the defaults set correctly
initGraph :: Ping -> Ping -> String ->  Graph TimeStamp Integer
initGraph p0 p1 host = namedListToGraph [p0,p1] host ("Time", "Ping (ms)") (host ++ ".ping")

--ping a host (Nothing is a failed ping)
getMaybePing :: String -> IO (Maybe Ping)
getMaybePing host = do
    t <- getTimeStamp --can't fail (lol?)
    p <- pingInt host
    case p of
        Just p' -> return $ Just (t,p')
        Nothing -> return Nothing

--gets a ping, will try 'pingRetry' times upon fails, if it fails 'pingRetry' times it will just crash 
getPing :: String -> IO Ping
getPing host = getPing_help host pingRetry

getPing_help :: String -> Int -> IO Ping
getPing_help host 0 = error $ "Failed to ping \"" ++ host ++ "\", tried " ++ (show pingRetry) ++ " times"
getPing_help host n = do
        p <- getMaybePing host
        case p of
            Just ping -> return ping
            Nothing   -> getPing_help host (n - 1)

--given a host name initialize a graph with two pings
getInitGraph :: String -> IO (Graph TimeStamp Integer)
getInitGraph host = do
    p0 <- getPing host
    p1 <- getPing host
    return $ initGraph p0 p1 host

--given a graph created by 'initGraph' add another ping to its set
addPing ::  IO (Graph TimeStamp Integer) -> IO (Graph TimeStamp Integer)
addPing graph = do
    g <- graph
    p <- getPing $ title g
    return $ addPoint p g

fakeGraph :: Graph TimeStamp Integer
fakeGraph = namedListToGraph fakeData "www.google.com" ("Date", "Ping (ms)") "Fake.ping"

fakeData :: [(TimeStamp, Integer)]
fakeData = [(1534342115.6511241,28),(1534342115.7470349,30),(1534342115.8073856,26),(1534342115.8670728,30),(1534342115.9174635,27),(1534342115.9774075,31),(1534342116.0369138,33),(1534342116.0970596,32)]