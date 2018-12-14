module Graph.Internal 
    (toInternal) 
where

import Graph.Types
import Graph.Build
import Utils
 
-- given an x, between a range a and b. Scale x so that it is the same ratio between a new range c and d.
-- where x = cur, a = min, b = max, c = newMin, d = newMax, and the result is scaled x
normalize :: RealFrac a => a -> a -> a -> a -> a -> a
normalize min max newMin newMax cur = (((newMax - newMin) * (cur - min)) / (max - min)) + newMin
  
-- normalize a set of points so that the new set has points scaled in x scaled between 0 and w, and y points are scaled between 0 and h
-- h = new y max
-- w = new x max
-- x = old min x
-- x' = old max x
-- y = old min y
-- y' = old max y
-- set = list of (x,y) points
normalizeSet :: (RealFrac a, RealFrac b) => b -> a -> a -> a -> b -> b -> [(a,b)] -> [(a,b)]
normalizeSet h w _ _  _ _ (_:[]) = [(w/2,h/2)]
normalizeSet h w x x' y y' set = mapA (normalize x x' 0 w) (normalize y y' 0 h) set

-- given a step size >0, and a list of points
-- this function finds the gradient between consecutive points
-- and between the two points will calculate intermediate points with a gap specified by the step size.
-- i.e. 
--  > getLines 1 [(1,1),(4,4)]
--  > [([(2,2),(3,3)], 1)] -- 2 points are found, and the two original points have a gradient of 1 between them
getLines :: (RealFrac a, Ord a, Enum a) => a -> [(a, a)] -> [([(a, a)], a)]
getLines stepSize ((x,y):(x',y'):xs) = [(points, m)] ++ (getLines stepSize ((x',y'):xs))
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
getLines _ _ = []
 
-- getAxis takes an axis, an integer which corresponds the size of the screen in that axis
-- then the min and max values for the axis, and it will return a list of points and axis labels
-- to be drawn
getAxis :: Axis -> Integer -> Integer ->  ([Integer],[a]) -> [(Integer, a)]
getAxis X len window (axispoints,plotpoints) = getAxis_help len window gap axispoints plotpoints 0 []
    where gap = len + 1
getAxis Y len window (axispoints,plotpoints) = getAxis_help len window gap axispoints plotpoints 0 []
    where gap = 1
    
getAxis_help :: Integer -> Integer -> Integer -> [Integer] -> [a] -> Integer -> [(Integer,a)] -> [(Integer,a)]
getAxis_help l w gap (x:xs) (p:ps) head acc | ((head + gap < x) && (x + l < w)) = getAxis_help l w gap xs ps x (acc ++ [(x,p)])
getAxis_help l w gap (x:xs) (p:ps) head acc | otherwise = getAxis_help l w gap xs ps head acc
getAxis_help _ _ _ [] [] _ acc = acc
          

--Take a graph and a window size, and create an internal graph which has a scaled set to the window size, and
--intermediate points to draw.
toInternalPure :: (RealFrac x, Enum x, Ord x, IOShow a, IOShow b) => 
    (a -> x) -> (b -> x) -> Integer -> Integer -> Graph a b -> Window Integer -> InternalGraph a b
toInternalPure cX cY lenX lenY g window = InternalGraph {
        graph = g,
        xAxisData = getAxis X lenX (width window) ((map left plotset),(map left $ dataSet g)),
        yAxisData = getAxis Y lenY (height window) (unzip $ mySort (\(x,_) (y,_) -> y - x) $ zip (map right plotset) (map right $ dataSet g)),
        plottingSet = plotset,
        lineSet = mapA (mapS round) (gradient) $ getLines 1 scaledSet,
        window = window
    }
    where
        h = fromIntegral $ height window --height must be the same type as the 'y' values
        w = fromIntegral $ width window --width must be the same type as the 'x' values
        scaledSet = normalizeSet h w (cX $ minX g) (cX $ maxX g) (cY $ minY g) (cY $ maxY g) (mapA cX cY $ dataSet g)
        plotset = mapS round scaledSet

gradient :: RealFrac a => a -> Char
--doesn't work great -_-
gradient x | x >= 8     = '|'
gradient x | x <= (-8)  = '|'
gradient x | x >= 2     = '/'
gradient x | x <= (-2)  = '\\'
gradient _              = '-'  
        
-- take a graph and scale it to be fit to the screen
toInternal :: (RealFrac x, Enum x, Ord x, IOShow a, IOShow b) =>
    (a -> x) -> (b -> x) -> Graph a b -> IO (Maybe (InternalGraph a b))
toInternal cX cY g = do
    lX <- ioShow (maxX g)
    lY <- ioShow (maxY g)
    let lenX = fromIntegral $ length lX
    let lenY = fromIntegral $ length lY
    s <- size
    case s of
        Just window -> return $ Just $ toInternalPure cX cY lenX lenY g (adjustSize window)
        Nothing -> return Nothing

-- the size function rounds up, so we round down by 1 to ensure our graph will not spill over
adjustSize :: Window Integer -> Window Integer
adjustSize win = Window {height = h, width = w}
    where h = (height win) - 1
          w = (width win) - 1