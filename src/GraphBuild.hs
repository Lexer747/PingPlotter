module GraphBuild () where

import InternalGraph
import Utils

--packs a list of (x,y) values into a graph object
listToGraph :: (Ord a, Ord b) => [(a,b)] -> Graph a b
listToGraph list = namedListToGraph list "untitled"

--given a list and a title it will pack the list into a graph object, setting the default min and max value based
--on the input of the list
namedListToGraph :: (Ord a, Ord b) => [(a,b)] -> String -> Graph a b
namedListToGraph list name = 
        Graph {
            maxX = xmax,
            minX = xmin,
            maxY = ymax,
            minY = ymin,
            title = name,
            dataSet = list
        }
    where
    (x,y) = unzip list
    (xmin, xmax) = getMinMax x
    (ymin, ymax) = getMinMax y

-- takes a list an returns a tuple containing the smallest and largest element
-- o(n) complexity
getMinMax :: Ord a => [a] -> (a,a)
getMinMax (x:xs) = getMinMax_ xs (x,x)

getMinMax_ :: Ord a => [a] -> (a,a) -> (a,a)
getMinMax_ (x:xs) (a,b) | x < a = getMinMax_ xs (x,b)
getMinMax_ (x:xs) (a,b) | x > b = getMinMax_ xs (a,x)
getMinMax_ (_:xs) acc           = getMinMax_  xs acc
getMinMax_ [] acc               = acc