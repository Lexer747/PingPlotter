module GraphTypes 
    (Graph(..), InternalGraph(..), Axis(..))
where

import System.Console.Terminal.Size
import Utils (left, right)

data Axis = X | Y deriving (Show)

-- a graph structure
data Graph a b = Graph {
        maxX :: a, --the largest value the x-axis can be
        minX :: a, --the smallest value the x-axis can be
        maxY :: b, --the largest value the y-axis can be
        minY :: b, --the smallest value the y-axis can be
        title :: String, --the title of the graph
        dataSet :: [(a,b)] -- the points in the graph
    }
       
instance (Show a, Show b) => Show (Graph a b) where
    show g = "Graph { \n   maxX = " ++ (show $ maxX g) ++ ", minX = " ++ (show $ minX g) ++ ", maxY = " ++ (show $ maxY g) ++ ", minY = " ++ (show $ minY g) ++ ",\n\
             \   title = " ++ (show $ title g) ++ ",\n\
             \   dataSet = " ++ (show $ dataSet g) ++ " \n}"
      
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
        xAxis :: [a],
        yAxis :: [a],
        scaledSet :: [(a,b)],
        lineSet :: [([(a,b)], c)],
        window :: Window Integer
    }

instance (Show a, Show b, Show c) => Show (InternalGraph a b c) where
    show g = "Internal Graph { \n   maxX = " ++ (show $ imaxX g) ++ ", minX = " ++ (show $ iminX g) ++ ", maxY = " ++ (show $ imaxY g) ++ ", minY = " ++ (show $ iminY g) ++ ",\n\
             \   title = " ++ (show $ ititle g) ++ ",\n\
             \   window = " ++ (show $ window g) ++ ", \n\
             \   baseSet = " ++ (show $ baseSet g) ++ ", \n\
             \   x-Axis [" ++ (show $ length $ xAxis g) ++ "] = " ++ (show $ xAxis g) ++ ", \n\
             \   y-Axis [" ++ (show $ length $ yAxis g) ++ "] = " ++ (show $ yAxis g) ++ ", \n\
             \   scaledSet = " ++ (show $ scaledSet g) ++ ", \n\
             \   lineSet = [\n" ++ (showLine $ lineSet g) ++ "   ]\n}"
             
showLine :: (Show a, Show b, Show c) => [([(a,b)], c)] -> String
showLine = concatMap (\(xs, c) -> "      (" ++ (show xs) ++ ",\n     " ++ (show c) ++ ") \n\n")