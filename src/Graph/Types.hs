module Graph.Types
    (module Graph.Types,
    Graph(..),
    InternalGraph(..),
    Axis(..),
    IOShow(..),
    module System.Console.Terminal.Size
    ) where
--store the relevant graph Types, also basic graph functions

import System.Console.Terminal.Size

--allow a different form of show
class IOShow a where
    ioShow :: a -> IO String

data Axis = X | Y deriving (Show)

-- a graph structure
data Graph a b = Graph {
        maxX :: a, --the largest value the x-axis can be
        minX :: a, --the smallest value the x-axis can be
        maxY :: b, --the largest value the y-axis can be
        minY :: b, --the smallest value the y-axis can be
        title :: String, --the title of the graph
        xAxis :: String, --name of x axis
        yAxis :: String, --name of y axis
        dataSet :: [(a,b)], -- the points in the graph
        saveLocation :: String -- the location to save the graph
    }

instance (Show a, Show b) => Show (Graph a b) where
    show g = "Graph { \n   maxX = " ++ (show $ maxX g) ++ ", minX = " ++ (show $ minX g) ++ ", maxY = " ++ (show $ maxY g) ++ ", minY = " ++ (show $ minY g) ++ ",\n\
             \   title = " ++ (show $ title g) ++ ",\n\
             \   axes = {" ++ (show $ xAxis g) ++ ", " ++ (show $ yAxis g) ++ "}, \n\ 
             \   dataSet = " ++ (show $ dataSet g) ++ " \n}"

-- a graph structure which also contains a another set of point which are the line segements for the original data set
-- a = type of x-axis
-- b = type of y-axis
data InternalGraph a b = InternalGraph {
        graph :: Graph a b, --the graph we want to represent
        xAxisData :: [(Integer,a)], --a selection of data points we plot on the axis to label it
        yAxisData :: [(Integer,b)], --same as above but for y
        plottingSet :: [(Integer,Integer)], --the scaled data set, fitting inside the window
        lineSet :: [([(Integer,Integer)], Char)], --the inbetween lines for each pair of points
        window :: Window Integer --the size of the window, everything is scaled to this
    }

instance (Show a, Show b) => Show (InternalGraph a b) where
    show g = "Internal Graph { \n   minX = " ++ (show $ minX $ graph g) ++ ", maxX = " ++ (show $ maxX $ graph g) ++ ", minY = " ++ (show $ minY $ graph g) ++ ", maxY = " ++ (show $ maxY $ graph g) ++ ",\n\
             \   title = " ++ (show $ title $ graph g) ++ ",\n\
             \   window = " ++ (show $ window g) ++ ", \n\
             \   dataSet [" ++ (show $ length $ dataSet $ graph g) ++ "] = " ++ (show $ dataSet $ graph g) ++ ", \n\
             \   x-Axis [" ++ (show $ length $ xAxisData g) ++ "] {" ++ (show $ xAxis $ graph g) ++ "} = " ++ (show $ xAxisData g) ++ ", \n\
             \   y-Axis [" ++ (show $ length $ yAxisData g) ++ "] {" ++ (show $ yAxis $ graph g) ++ "} = " ++ (show $ yAxisData g) ++ ", \n\
             \   scaledSet = " ++ (show $ plottingSet g) ++ ", \n\
             \   lineSet = [\n" ++ (showLine $ lineSet g) ++ "   ]\n"
             
showLine :: (Show a, Show b, Show c) => [([(a,b)], c)] -> String
showLine = concatMap (\(xs, c) -> "      (" ++ (show xs) ++ ",\n     " ++ (show c) ++ ") \n\n")