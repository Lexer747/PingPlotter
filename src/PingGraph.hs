module PingGraph where

import PingTypes
import PingAPI
import GraphTypes
import GraphBuild (namedListToGraph, addPoint)

--given two starting pings create a graph with all the defaults set correctly
initGraph :: Ping -> Ping -> String ->  Graph TimeStamp Integer
initGraph p0 p1 host = namedListToGraph [p0,p1] host ("Time", "Ping (ms)")

--ping a host
getPing :: String -> IO Ping
getPing host = do
    t <- getTimeStamp
    p <- pingInt host
    return (t,p)

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
fakeGraph = Graph {
        maxX = 1534342116.0970596, minX = 1534342115.6511241, maxY = 33, minY = 26,
        title = "www.google.com",
        xAxis = "Date",
        yAxis = "Ping (ms)",
        dataSet = [(1534342115.6511241,28),(1534342115.7470349,30),(1534342115.8073856,26),(1534342115.8670728,30),(1534342115.9174635,27),(1534342115.9774075,31),(1534342116.0369138,33),(1534342116.0970596,32)]
    }