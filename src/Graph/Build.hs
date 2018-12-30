module Graph.Build where
--this module will build a graph Type from input

import Graph.Types
import Utils (getMinMax)

--packs a list of (x,y) values into a graph object
listToGraph :: (Enum a, Ord a, Enum b, Ord b) => [(a,b)] -> Graph a b
listToGraph list = namedListToGraph list "untitled" ("unnamed-x","unnamed-y") "Untitled.ping"

--given a list and a title it will pack the list into a graph object, setting the default min and max value based
--on the input of the list
namedListToGraph :: (Enum a, Ord a, Enum b, Ord b) => 
    [(a,b)] -> String -> (String, String) -> String -> Graph a b
namedListToGraph list name (xaxis,yaxis) file = Graph {
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
    where
        (x,y) = unzip list
        (xmin, xmax) = ensureGap $ getMinMax x
        (ymin, ymax) = ensureGap $ getMinMax y

ensureGap :: (Enum a, Ord a) => (a,a) -> (a,a)
ensureGap (min,max) | min == max            = (pred min, succ max) --if equal, make a gap of 1
ensureGap (min,max) | ((succ min) == max)   = (min, succ max) --if gap of 0, make a gap of 1
ensureGap otherwise                         = otherwise

-- edit the list of points in the graph with the function, and rebuild the constraint
-- slow as a whole new graph is made, so an O(n) search occurs for every call
editGraph :: (Enum a, Ord a, Enum b, Ord b) => ([(a,b)] -> [(a,b)]) -> Graph a b -> Graph a b
editGraph func graph = namedListToGraph newSet (title graph) (xAxis graph, yAxis graph) (saveLocation graph)
    where
        newSet = func (dataSet graph)

-- some examples of edit graph:

-- add a point to a graph
addPoint :: (Enum a, Ord a, Enum b, Ord b) => (a,b) -> Graph a b -> Graph a b
addPoint point = editGraph (++ [point])

-- add a list of points to a graph
addPoints :: (Enum a, Ord a, Enum b, Ord b) => [(a,b)] -> Graph a b -> Graph a b
addPoints points = editGraph (++ points)