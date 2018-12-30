module Ping.Types where
-- All the types we need to store Pings

import Graph.Types
import Graph.Build
import Ping.API
import Utils (wordsWhen)

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

-- encapsulate the POSIXTime Stamp so we can change the show function hehe
-- probably a better way than this
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

instance IOShow TimeStamp where
    ioShow = localizeTimeStamp

instance IOShow Integer where
    ioShow a = return $ show a --wrapper

--convert a utc time into its timezoned one
localizeTimeStamp :: TimeStamp -> IO String
localizeTimeStamp (MkTimeStamp t) = do
    z <- getCurrentTimeZone --get the timezone of the user
    --then show that time in out timezone, and strip the date and milliseconds
    let stripped = head $ wordsWhen (=='.') $ (!!) (wordsWhen (==' ') $ show $ utcToLocalTime z utc) 1
    return $ " " ++ stripped ++ " "
        where utc = posixSecondsToUTCTime t

--get the current POSIX time
getTimeStamp :: IO TimeStamp
getTimeStamp = do
    t <- getPOSIXTime
    return $ MkTimeStamp t

type Ping = (TimeStamp, Integer)

