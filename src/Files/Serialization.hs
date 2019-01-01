module Files.Serialization
    (saveGraph, readPingGraph)
where

import Graph.Types
import Ping.Types

import Data.Binary
import Data.Time.Clock (nominalDiffTimeToSeconds, secondsToNominalDiffTime)

--get the Pico number out of the Timestamp so we can serialize that
instance Binary TimeStamp where
    put (MkTimeStamp a) = do
        put $ nominalDiffTimeToSeconds a
    get = do
        a <- get
        return $ MkTimeStamp $ secondsToNominalDiffTime a

--since all the primitives are already serializable simply do each one in turn
instance (Binary a, Binary b) => Binary (Graph a b) where
    put g = do
        put (maxX g)
        put (minX g)
        put (maxY g)
        put (minY g)
        put (title g)
        put (xAxis g)
        put (yAxis g)
        put (dataSet g)
        put (saveLocation g)
    get = do
        xmax <- get
        xmin <- get
        ymax <- get
        ymin <- get
        name <- get
        xaxis <- get
        yaxis <- get
        list <- get
        file <- get
        return Graph {
                maxX = xmax,
                minX = xmin,
                maxY = ymax,
                minY = ymin,
                title = name,
                xAxis = xaxis,
                yAxis = yaxis,
                dataSet = list,
                saveLocation = file
            }

--given a graph which implements serialization save it too a file 
saveGraph :: (Binary a, Binary b) => Graph a b -> IO ()
saveGraph g = encodeFile (saveLocation g) g

--given a path, read it and put it back in the graph object
readGraph :: (Binary a, Binary b) => String -> IO (Graph a b)
readGraph s = do
            r <- decodeFileOrFail s
            case r of
                Right g -> return g
                Left (_,err) -> error err --TODO

--tell the compiler which type to convert the bytes to
readPingGraph :: String -> IO (Graph TimeStamp Integer)
readPingGraph = readGraph