module Graph.Internal
    (toInternal
    , filterAxis
    ) where

import Graph.Types
import Utils

import Prelude hiding (min, max)

-- given an x, between a range a and b. Scale x so that it is the same ratio between a new range c and d.
-- where x = cur, a = min, b = max, c = newMin, d = newMax, and the result is scaled x
normalize :: RealFrac a => a -> a -> a -> a -> a -> a
normalize min max _ _ _ | min == max = error "normalize called with equal old min max"
normalize min max newMin newMax cur = (((newMax - newMin) * (cur - min)) / (max - min)) + newMin


processSet :: (RealFrac x, Enum x, Ord x) =>
    (a -> x) -> (b -> x) -> x -> x -> a -> a -> b -> b -> [(a,b)] -> [((x,x),(a,b))]
processSet _ _ h w _ _ _ _ ((a,b):[]) = [((w/2,h/2),(a,b))] 
    -- ^ if a set has exactly one point [edge case]
    --then we don't care about scaling it, just put it in the middle of the window
processSet cX cY h w x x' y y' set = processSetHelp cX cY h w x x' y y' set []

processSetHelp :: (RealFrac x, Enum x, Ord x) =>
    (a -> x) -> (b -> x) ->
    x -> x -> a -> a -> b -> b -> [(a,b)] -> [((x,x),(a,b))] -> [((x,x),(a,b))] 
processSetHelp _ _ _ _ _ _ _ _ [] acc = acc --base case
processSetHelp cX cY h w x x' y y' ((a,b):set) acc = processSetHelp cX cY h w x x' y y' set newAcc
    where newX = normalize (cX x) (cX x') 1 (w - 1) (cX a) --scale the x value
          newY = normalize (cY y) (cY y') 1 (h - 1) (cY b) --scale the y value
          newAcc = acc ++ [((newX, newY),(a,b))] --combine the new scaled values with the old values

--map round over all the scaled points
roundProcessedSet :: (RealFrac x, Enum x, Ord x) =>
    [((x,x),(a,b))] -> [((Integer,Integer),(a,b))]
roundProcessedSet = mapA (symmetric round) id

-- given a step size >=0, and a list of points
-- this function finds the gradient between consecutive points
-- and between the two points will calculate intermediate points with a gap specified by the step size.
-- i.e. 
--  > getLines 1 [(1,1),(4,4)]
--  > [([(2,2),(3,3)], 1)] -- 2 points are found, and the two original points have a gradient of 1 between them
getLines :: (RealFrac a, Ord a, Enum a) => a -> [(a, a)] -> [([(a, a)], a)]
getLines stepSize _ | stepSize <= 0   = error "getLines called with stepSize <= 0"
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

-- filter out the points which are too close too each other, or spill off the end
filterAxis :: Axis -> Integer -> Integer -> [(Integer,a)] -> [(Integer,a)]
filterAxis Y _   wndw points = filterAxisHelp 1 wndw 1 sorted []
    where sorted = mySort (\(x,_) (y,_) -> x > y) points
filterAxis X len wndw points = filterAxisHelp len wndw 1 sorted []
    where sorted = mySort (\(x,_) (y,_) -> x > y) points

filterAxisHelp :: Integer -> Integer -> Integer -> [(Integer,a)] -> [(Integer,a)] -> [(Integer,a)]
filterAxisHelp _ _ _ [] acc                   = acc
filterAxisHelp gap wndw prev ((p,a):ps) acc =
    if (p > (prev + gap)) && ((p + gap) <= wndw)
        then filterAxisHelp gap wndw p ps (acc ++ [(p,a)])
        else filterAxisHelp gap wndw prev ps acc

--Take a graph and a window size, and create an internal graph which has a scaled set to the window size, and
--intermediate points to draw.
toInternalPure :: (RealFrac x, Enum x, Ord x, IOShow a, IOShow b) => 
    (a -> x) -> (b -> x) -> Integer -> Integer -> Graph a b -> Window Integer -> InternalGraph a b
toInternalPure cX cY lenX lenY g wndw = InternalGraph {
        graph = g,
        xAxisData = filterAxis X lenX (width wndw) xAxisDataList,
        yAxisData = filterAxis Y lenY (height wndw) yAxisDataList,
        plottingSet = plotset,
        lineSet = mapA (mapS round) (gradient) $ getLines 1 scaledSet,
        window = wndw
    }
    where
        h = fromIntegral $ height wndw --height must be the same type as the 'y' values
        w = fromIntegral $ width wndw --width must be the same type as the 'x' values
        processedSet = processSet cX cY h w (minX g) (maxX g) (minY g) (maxY g) (dataSet g)
        -- ^ scales the points in the graph to fit inside the window
        roundedSet = roundProcessedSet processedSet --rounds the scaled points to fit on specific pixels
        scaledSet = map left processedSet --take only the scaled points
        plotset = map left roundedSet --take only the rounded scaled points
        xAxisDataList = mapA left left roundedSet 
        -- ^match the rounded scaled points with the original point which created it, x axis specific
        yAxisDataList = mapA right right roundedSet --same as above but for y

gradient :: RealFrac a => a -> Char
-- ^doesn't work great -_-
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
    s <- size --from System.Console.Terminal.Size
    case s of
        Just wndw -> return $ Just $ toInternalPure cX cY lenX lenY g (adjustSize wndw)
        Nothing -> return Nothing

-- the size function rounds up, so we round down by 1 to ensure our graph will not spill over
adjustSize :: Window Integer -> Window Integer
adjustSize win = Window {height = h, width = w}
    where h = (height win) - 2
          w = (width win) - 2