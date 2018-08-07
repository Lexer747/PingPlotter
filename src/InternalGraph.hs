module InternalGraph where
    
import System.Console.Terminal.Size

import Utils

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
data InternalGraph a b c = InternalGraph {
        imaxX :: a,
        iminX :: a,
        imaxY :: b,
        iminY :: b,
        ititle :: String,
        idataSet :: [(a,b)],
        ilineSet :: [([(a,b)], c)]
    }
        deriving (Show)
 
-- given an x, between a range a and b. Scale x so that it is the same ratio between a new range c and d.
-- where x = cur, a = min, b = max, c = newMin, d = newMax, and the result is scaled x
normalize :: Fractional a => a -> a -> a -> a -> a -> a
normalize cur min max newMin newMax = (((newMax - newMin) * (cur - min)) / (max - min)) + newMin

normalizeInt :: Integral a => a -> a -> a -> a -> a -> a
normalizeInt cur min max newMin newMax = (((newMax - newMin) * (cur - min)) `div` (max - min)) + newMin

--normalize a graph to a window size
normalizeGraph :: InternalGraph Int Int Float -> Window Int -> InternalGraph Int Int Float
normalizeGraph graph win =
    InternalGraph {
        iminX = (iminX graph),
        imaxX = (imaxX graph),
        iminY = (iminY graph),
        imaxY = (imaxY graph),
        ititle = (ititle graph),
        idataSet = newDataSet,
        ilineSet = newLineSet
    }
    where
        h = height win
        w = width win
        f xs = normalizeSet h w (iminX graph) (imaxX graph) (iminY graph) (imaxY graph) xs
        newDataSet = f (idataSet graph)
        newLineSet = map (\(a,b) -> (f a, b)) (ilineSet graph)
        
-- normalize a set of ints so that the new set has points scaled in x scaled between 0 and w, and y points are scaled between 0 and h
-- h = new y max
-- w = new x max
-- x = old min x
-- x' = old max x
-- y = old min y
-- y' = old max y
-- set = list of (x,y) points
normalizeSet :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
normalizeSet h w x x' y y' set = map (\(n,m)-> ((normalizeInt n x x' 0 w), (normalizeInt m y y' 0 h))) set
        
-- take a graph and scale it to be fit to the screen
fitToScreen :: InternalGraph Int Int Float -> IO (Maybe (InternalGraph Int Int Float, Window Int))
fitToScreen g = do
                    s <- size
                    case s of
                        Just window -> return $ Just $ (normalizeGraph g window, window)
                        Nothing -> return Nothing

toInternal :: Graph Int Int -> InternalGraph Int Int Float
toInternal g = InternalGraph { 
        iminX = (minX g),
        imaxX = (maxX g),
        iminY = (minY g),
        imaxY = (maxY g),
        ititle = (title g),
        idataSet = (dataSet g),
        ilineSet = getLines (dataSet g)
    }
   

getLines :: [(Int,Int)] -> [([(Int,Int)], Float)]
getLines xs = map (\(a,b) -> (unique $ map (\(x,y) -> (round x, round y)) a, b)) getPoints
    where
        getPoints = getLinesPrecise 1 toFloat
        toFloat = map (\(x,y) -> (fromIntegral x, fromIntegral y)) xs

-- given a step size >0, and a list of points
-- this function finds the gradient between consecutive points
-- and between the two points will calculate intermediate points with a gap specified by the step size.
-- i.e. 
--  > getLinesPrecise 1 [(1,1),(3,3)]
--  > [([(2,2)], 1)] -- a single point is found, and the two points have a gradient of 1 between them
getLinesPrecise :: Float -> [(Float,Float)] -> [([(Float,Float)], Float)]
getLinesPrecise stepSize ((x,y):(x',y'):xs) = [(points, m)] ++ (getLinesPrecise stepSize ((x',y'):xs))
    where
        points = xpoints ++ ypoints
        xpoints = map (\n -> (n, f n)) [(x+1),(x+1+stepSize)..(x' - 1)] 
        ypoints = map (\n -> (g n, n)) [(y+1),(y+1+stepSize)..(y' - 1)]
        g n = (n - b) / m
        f n = (n * m) + b
        b = y - (x * m)
        m = (y' - y) / (x' - x)
getLinesPrecise _ (_:_) = []
getLinesPrecise _ [] = []