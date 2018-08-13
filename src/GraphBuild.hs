module GraphBuild where

import GraphTypes
import Utils 

--packs a list of (x,y) values into a graph object
listToGraph :: Ord a => [(a,a)] -> Graph a a
listToGraph list = namedListToGraph list "untitled" ("unnamed-x","unnamed-y")

--given a list and a title it will pack the list into a graph object, setting the default min and max value based
--on the input of the list
namedListToGraph :: Ord a => [(a,a)] -> String -> (String, String) -> Graph a a
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
editGraph :: (Ord a, Ord b) => ([(a,a)] -> [(b,b)]) -> Graph a a -> Graph b b
editGraph func graph = namedListToGraph newSet (title graph) (xAxis graph, yAxis graph)
    where 
        newSet = func (dataSet graph)
        
-- some examples of edit graph:

-- add a point to a graph
addPoint :: Ord a => (a,a) -> Graph a a -> Graph a a
addPoint point = editGraph (++ [point])

-- add a list of points to a graph
addPoints :: Ord a => [(a,a)] -> Graph a a -> Graph a a
addPoints points = editGraph (++ points)

--test graph
demo = namedListToGraph (zip [1..50] [1,5,9,10,7,8,3,4,7,2,0,10]) "Demo" ("x-points","y|points")

--sample graphs
tanDemo = namedListToGraph (zip [1..50] (map (\x -> round $ (tan x) * 100) [0,0.2..])) "tan(x) * 100" ("x","y")
sinDemo = namedListToGraph (zip [1..50] (map (\x -> round $ (sin x) * 100) [0,0.2..])) "sin(x) * 100" ("x","y")