module InternalGraph 
    (toInternal, toInternalInt, xaxisGap, yaxisGap) 
where
    
import System.Console.Terminal.Size
import GraphTypes
import GraphBuild
import Utils
 
-- given an x, between a range a and b. Scale x so that it is the same ratio between a new range c and d.
-- where x = cur, a = min, b = max, c = newMin, d = newMax, and the result is scaled x
normalize :: Fractional a => a -> a -> a -> a -> a -> a
normalize cur min max newMin newMax = (((newMax - newMin) * (cur - min)) / (max - min)) + newMin
  
-- normalize a set of ints so that the new set has points scaled in x scaled between 0 and w, and y points are scaled between 0 and h
-- h = new y max
-- w = new x max
-- x = old min x
-- x' = old max x
-- y = old min y
-- y' = old max y
-- set = list of (x,y) points
normalizeSet :: Fractional a => a -> a -> a -> a -> a -> a -> [(a,a)] -> [(a,a)]
normalizeSet h w _ _  _ _ (_:[]) = [(w/2,h/2)]
normalizeSet h w x x' y y' set = map (\(n,m)-> ((normalize n x x' 0 w), (normalize m y y' 0 h))) set

-- given a step size >0, and a list of points
-- this function finds the gradient between consecutive points
-- and between the two points will calculate intermediate points with a gap specified by the step size.
-- i.e. 
--  > getLinesPrecise 1 [(1,1),(4,4)]
--  > [([(2,2),(3,3)], 1)] -- 2 points are found, and the two original points have a gradient of 1 between them
getLinesPrecise :: (Ord a, Fractional a, Enum a) => a -> [(a,a)] -> [([(a,a)], a)]
getLinesPrecise stepSize ((x,y):(x',y'):xs) = [(points, m)] ++ (getLinesPrecise stepSize ((x',y'):xs))
    where
        points = xpoints ++ ypoints --combine all possible points which lie inbetween our original points
        xpoints = map (\n -> (n, f n)) $ if x < x' --map our x = (y - c) / m over every point between our given points
                    then [(x+stepSize),(x+(2*stepSize))..(x' - stepSize)] 
                    else [(x'+stepSize),(x'+(2*stepSize))..(x - stepSize)] 
        ypoints = map (\n -> (g n, n)) $ if y < y' --map our y = mx + c over every point between our given points
                    then [(y+stepSize),(y+(2*stepSize))..(y' - stepSize)]
                    else [(y'+stepSize),(y'+(2*stepSize))..(y - stepSize)]
        g n = (n - b) / m -- x = (y - c) / m, where n is our y and g = x
        f n = (n * m) + b -- y = mx + c, where n is our x and f = y
        b = y - (x * m) --find the intersect (y = mx + c)
        m = (y' - y) / (x' - x) --find the gradient (rise / run)
getLinesPrecise _ _ = []

--generates from a min max tuple, a list of points which represent that axis
getAxisPrecise :: (Ord a, Fractional a, Enum a) => Axis -> (a,a) -> a -> [a]
getAxisPrecise X (min,max) numberOfPoints = map left $ left $ head $ getLinesPrecise ((max - min) / numberOfPoints) [(min,0),(max,0)]
getAxisPrecise Y (min,max) numberOfPoints = map right $ left $ head $ getLinesPrecise ((max - min) / numberOfPoints) [(0,min),(0,max)]

xaxisGap, yaxisGap :: Num a => a
xaxisGap = 8 --the number of spaces between an interval
yaxisGap = 4 --the number of spaces between an interval

--Take a graph and a window size, and create an internal graph which has a scaled set to the window size, and
--intermediate points to draw.
--toInternalPure :: (Ord a, Fractional a, Enum a, Ord b, Fractional b, Enum b) => Graph a b -> Window Integer -> InternalGraph a b c
toInternalPure graph window = InternalGraph {
        graph = graph,
        xAxisData = [] --(getAxisPrecise X ((minX graph),(maxX graph)) (w / xaxisGap)),
        yAxisData = [] --(getAxisPrecise Y ((minY graph),(maxY graph)) (h / yaxisGap)),
        plottingSet = scaledSet,
        lineSet = getLinesPrecise 1 scaledSet,
        window = window
    }
    where 
        h = fromIntegral $ height window
        w = fromIntegral $ width window
        scaledSet = normalizeSet h w (minX graph) (maxX graph) (minY graph) (maxY graph) (dataSet graph)

-- take a graph and scale it to be fit to the screen
--toInternal :: (Ord a, Fractional a, Enum a) => Graph a a -> IO (Maybe (InternalGraph a a a))
toInternal g = do
                s <- size
                case s of
                    Just window -> return $ Just $ toInternalPure g (adjustSize window)
                    Nothing -> return Nothing
                       
-- the size function rounds up, so we round down by 1 to ensure our graph will not spill over
adjustSize :: Window Integer -> Window Integer
adjustSize win = Window {height = h, width = w}
    where h = (height win) - 1
          w = (width win) - 1
