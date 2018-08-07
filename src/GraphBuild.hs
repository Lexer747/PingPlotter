module GraphBuild (
    listToGraph,
    namedListToGraph,
) where

import GraphTypes
import Utils 

--packs a list of (x,y) values into a graph object
listToGraph :: (Ord a, Ord b) => [(a,b)] -> Graph a b
listToGraph list = namedListToGraph list "untitled"

--given a list and a title it will pack the list into a graph object, setting the default min and max value based
--on the input of the list
namedListToGraph :: (Ord a, Ord b) => [(a,b)] -> String -> Graph a b
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
editGraph :: (Ord a, Ord b) => ([(a,b)] -> [(a,b)]) -> Graph a b -> Graph a b
editGraph func graph = namedListToGraph newSet (title graph)
    where 
        newSet = func (dataSet graph)
        
-- some examples of edit graph:

-- add a point to a graph
addPoint :: (Ord a, Ord b) => (a,b) -> Graph a b -> Graph a b
addPoint point = editGraph (++ [point])

-- add a list of points to a graph
addPoints :: (Ord a, Ord b) => [(a,b)] -> Graph a b -> Graph a b
addPoints points = editGraph (++ points)