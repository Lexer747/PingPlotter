module TestData where

import GraphTypes
import Utils 
import Data.Dates 

baseDate = DateTime {
        year = 2018,
        month = 1,
        day = 1,
        hour = 1,
        minute = 1,
        second = 1
    }
    


fakeDateList :: Time -> DateTime -> [DateTime]
fakeDateList f date = [next] ++ (fakeDateList f next)
    where next = addTime date f

{-
--test graph
demo = namedListToGraph (zip [1..50] [1,5,9,10,7,8,3,4,7,2,0,10]) "Demo" ("x-points","y|points")

--sample graphs
tanDemo = namedListToGraph (zip [1..50] (map (\x -> round $ (tan x) * 100) [0,0.2..])) "tan(x) * 100" ("x","y")
sinDemo = namedListToGraph (zip [1..50] (map (\x -> round $ (sin x) * 100) [0,0.2..])) "sin(x) * 100" ("x","y")
-}