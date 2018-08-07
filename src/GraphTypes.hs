module GraphTypes where

import System.Console.Terminal.Size

-- a graph structure
data Graph a b = Graph {
        maxX :: a, --the largest value the x-axis can be
        minX :: a, --the smallest value the x-axis can be
        maxY :: b, --the largest value the y-axis can be
        minY :: b, --the smallest value the y-axis can be
        title :: String, --the title of the graph
        dataSet :: [(a,b)] -- the points in the graph
    }
        deriving (Show)
            
-- a graph structure which also contains a another set of point which are the line segements for the original data set
-- a = type of x-axis
-- b = type of y-axis
-- c = type of calculated gradient
data InternalGraph a b c = InternalGraph {
        imaxX :: a,
        iminX :: a,
        imaxY :: b,
        iminY :: b,
        ititle :: String,
        baseSet :: [(a,b)],
        scaledSet :: [(a,b)],
        lineSet :: [([(a,b)], c)],
        window :: Window Integer
    }
        deriving (Show)