module GraphTypes 
    (Graph(..), InternalGraph(..), Axis(..), IOShow(..))
where

import System.Console.Terminal.Size
import Control.DeepSeq (NFData(..))

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
        dataSet :: [(a,b)] -- the points in the graph
    }
  
instance (NFData a, NFData b) => NFData (Graph a b) where
    rnf g = rnf (maxX g) `seq`
                rnf (minX g) `seq`
                rnf (maxY g) `seq`
                rnf (minY g) `seq`
                rnf (title g) `seq`
                rnf (xAxis g) `seq`
                rnf (yAxis g) `seq`
                rnf (dataSet g)
       
instance (Show a, Show b) => Show (Graph a b) where
    show g = "Graph { \n   maxX = " ++ (show $ maxX g) ++ ", minX = " ++ (show $ minX g) ++ ", maxY = " ++ (show $ maxY g) ++ ", minY = " ++ (show $ minY g) ++ ",\n\
             \   title = " ++ (show $ title g) ++ ",\n\
             \   axes = {" ++ (show $ xAxis g) ++ ", " ++ (show $ yAxis g) ++ "}, \n\ 
             \   dataSet = " ++ (show $ dataSet g) ++ " \n}"
      
-- a graph structure which also contains a another set of point which are the line segements for the original data set
-- a = type of x-axis
-- b = type of y-axis
data InternalGraph a b = InternalGraph {
        graph :: Graph a b,
        xAxisData :: [(Integer,a)],
        yAxisData :: [(Integer,b)],
        plottingSet :: [(Integer,Integer)],
        lineSet :: [([(Integer,Integer)], Char)],
        window :: Window Integer
    }
  
instance (NFData a) => NFData (Window a) where
    rnf w = rnf (height w) `seq` rnf (width w)
    
instance (NFData a, NFData b) => NFData (InternalGraph a b) where
    rnf g = rnf (graph g) `seq`
                rnf (xAxisData g) `seq`
                rnf (yAxisData g) `seq`
                rnf (plottingSet g) `seq`
                rnf (lineSet g) `seq`
                rnf (window g)

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