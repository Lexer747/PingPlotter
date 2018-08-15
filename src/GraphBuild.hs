module GraphBuild where

import GraphTypes
import Utils

--packs a list of (x,y) values into a graph object
listToGraph :: (Ord a, Ord b) => [(a,b)] -> Graph a b
listToGraph list = namedListToGraph list "untitled" ("unnamed-x","unnamed-y")

--given a list and a title it will pack the list into a graph object, setting the default min and max value based
--on the input of the list
namedListToGraph :: (Ord a, Ord b) => [(a,b)] -> String -> (String, String) -> Graph a b
namedListToGraph list name (xaxis,yaxis)  = Graph {
            maxX = xmax,
            minX = xmin,
            maxY = ymax,
            minY = ymin,
            title = name,
            xAxis = xaxis,
            yAxis = yaxis,
            dataSet = list
        }
    where
    (x,y) = unzip list
    (xmin, xmax) = getMinMax x
    (ymin, ymax) = getMinMax y
   
-- edit the list of points in the graph with the function, and rebuild the constraint
editGraph :: (Ord a, Ord b, Ord c, Ord d) => ([(a,b)] -> [(c,d)]) -> Graph a b -> Graph c d
editGraph func graph = namedListToGraph newSet (title graph) (xAxis graph, yAxis graph)
    where 
        newSet = func (dataSet graph)
        
-- some examples of edit graph:

-- add a point to a graph
addPoint :: (Ord a, Ord b) => (a,b) -> Graph a b -> Graph a b
addPoint point = editGraph (++ [point])

-- add a list of points to a graph
addPoints :: (Ord a, Ord b) => [(a,b)] -> Graph a b -> Graph a b
addPoints points = editGraph (++ points)