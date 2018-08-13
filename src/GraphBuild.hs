module GraphBuild where

import GraphTypes
import Utils 

--packs a list of (x,y) values into a graph object
listToGraph :: Ord a => [(a,a)] -> Graph a a
listToGraph list = namedListToGraph list "untitled"

--given a list and a title it will pack the list into a graph object, setting the default min and max value based
--on the input of the list
namedListToGraph :: Ord a => [(a,a)] -> String -> Graph a a
namedListToGraph list name = Graph {
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
   
-- edit the list of points in the graph with the function, and rebuild the constraint
editGraph :: (Ord a, Ord b) => ([(a,a)] -> [(b,b)]) -> Graph a a -> Graph b b
editGraph func graph = namedListToGraph newSet (title graph)
    where 
        newSet = func (dataSet graph)
        
-- some examples of edit graph:

-- add a point to a graph
addPoint :: Ord a => (a,a) -> Graph a a -> Graph a a
addPoint point = editGraph (++ [point])

-- add a list of points to a graph
addPoints :: Ord a => [(a,a)] -> Graph a a -> Graph a a
addPoints points = editGraph (++ points)

--sample graph with outliers
demo = namedListToGraph (zip [1..100] [10]) "Demo"