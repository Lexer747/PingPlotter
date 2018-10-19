module PingTypes where

import GraphTypes
import GraphBuild
import PingAPI
import Data.Time.Clock
import Data.Time.Clock.POSIX

-- encapsulate the POSIXTime Stamp so we can change the show function
newtype TimeStamp = MkTimeStamp POSIXTime deriving (Eq,Ord)

--- ALL implementations from 'Data.Time.Clock.Internal.NominalDiffTime' ----
instance Enum TimeStamp where
    succ (MkTimeStamp a) = MkTimeStamp (succ a)
    pred (MkTimeStamp a) = MkTimeStamp (pred a)
    toEnum = MkTimeStamp . toEnum
    fromEnum (MkTimeStamp a) = fromEnum a
    enumFrom (MkTimeStamp a) = fmap MkTimeStamp (enumFrom a)
    enumFromThen (MkTimeStamp a) (MkTimeStamp b) = fmap MkTimeStamp (enumFromThen a b)
    enumFromTo (MkTimeStamp a) (MkTimeStamp b) = fmap MkTimeStamp (enumFromTo a b)
    enumFromThenTo (MkTimeStamp a) (MkTimeStamp b) (MkTimeStamp c) = fmap MkTimeStamp (enumFromThenTo a b c)

instance Num TimeStamp where
    (MkTimeStamp a) + (MkTimeStamp b) = MkTimeStamp (a + b)
    (MkTimeStamp a) - (MkTimeStamp b) = MkTimeStamp (a - b)
    (MkTimeStamp a) * (MkTimeStamp b) = MkTimeStamp (a * b)
    negate (MkTimeStamp a) = MkTimeStamp (negate a)
    abs (MkTimeStamp a) = MkTimeStamp (abs a)
    signum (MkTimeStamp a) = MkTimeStamp (signum a)
    fromInteger i = MkTimeStamp (fromInteger i)

instance Real TimeStamp where
    toRational (MkTimeStamp a) = toRational a

instance Fractional TimeStamp where
    (MkTimeStamp a) / (MkTimeStamp b) = MkTimeStamp (a / b)
    recip (MkTimeStamp a) = MkTimeStamp (recip a)
    fromRational r = MkTimeStamp (fromRational r)

instance RealFrac TimeStamp where
    properFraction (MkTimeStamp a) = (i,MkTimeStamp f) where
        (i,f) = properFraction a
    truncate (MkTimeStamp a) = truncate a
    round (MkTimeStamp a) = round a
    ceiling (MkTimeStamp a) = ceiling a
    floor (MkTimeStamp a) = floor a

------------------------------------------------------------------------------

instance Show TimeStamp where
    show (MkTimeStamp a) = show $ posixSecondsToUTCTime a
    
--get the current posix time
getTimeStamp :: IO TimeStamp
getTimeStamp = do
                   t <- getPOSIXTime
                   return $ MkTimeStamp t
    
type Ping = (TimeStamp, Integer)

--given two starting pings create a graph with all the defaults set correctly
initGraph :: Ping -> Ping -> String ->  Graph TimeStamp Integer
initGraph p0 p1 host = namedListToGraph [p0,p1] host ("Date", "Ping (ms)") 

--ping a host
getPing :: String -> IO Ping
getPing host = do
                  t <- getTimeStamp
                  p <- pingInt host
                  return (t,p)
                  
            
--given a host name initalize a graph with two pings            
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
               maxX = 1534342116.0970596, minX = 1534342115.6511241, maxY = 32, minY = 29,
               title = "www.google.com",
               xAxis = "Date",
               yAxis = "Ping (ms)",
               dataSet = [(1534342115.6511241,29),(1534342115.7470349,30),(1534342115.8073856,29),(1534342115.8670728,30),(1534342115.9174635,29),(1534342115.9774075,29),(1534342116.0369138,29),(1534342116.0970596,32)]
            }
         
