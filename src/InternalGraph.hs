module InternalGraph (toInternal, toInternalInt) where
    
import System.Console.Terminal.Size
import GraphTypes
import GraphBuild
import Utils
 
-- given an x, between a range a and b. Scale x so that it is the same ratio between a new range c and d.
-- where x = cur, a = min, b = max, c = newMin, d = newMax, and the result is scaled x
normalize :: Fractional a => a -> a -> a -> a -> a -> a
normalize cur min max newMin newMax = (((newMax - newMin) * (cur - min)) / (max - min)) + newMin

normalizeInt :: Integral a => a -> a -> a -> a -> a -> a
normalizeInt cur min max newMin newMax = (((newMax - newMin) * (cur - min)) `div` (max - min)) + newMin
        
-- normalize a set of ints so that the new set has points scaled in x scaled between 0 and w, and y points are scaled between 0 and h
-- h = new y max
-- w = new x max
-- x = old min x
-- x' = old max x
-- y = old min y
-- y' = old max y
-- set = list of (x,y) points
normalizeSet :: Fractional a => a -> a -> a -> a -> a -> a -> [(a,a)] -> [(a,a)]
normalizeSet h w x x' y y' set = map (\(n,m)-> ((normalize n x x' 0 w), (normalize m y y' 0 h))) set

normalizeIntSet :: Integral a => a -> a -> a -> a -> a -> a -> [(a,a)] -> [(a,a)]
normalizeIntSet h w x x' y y' set = map (\(n,m)-> ((normalizeInt n x x' 0 w), (normalizeInt m y y' 0 h))) set
 
-- does getLinesPrecise but rounds the values and has a fixed step of 1
getLines :: (RealFrac b, Enum b, Integral a) => [(a,a)] -> [([(a, a)], b)]
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
getLinesPrecise :: (Fractional a, Enum a) => a -> [(a,a)] -> [([(a,a)], a)]
getLinesPrecise stepSize ((x,y):(x',y'):xs) = [(points, m)] ++ (getLinesPrecise stepSize ((x',y'):xs))
    where
        points = xpoints ++ ypoints
        xpoints = map (\n -> (n, f n)) [(x+1),(x+1+stepSize)..(x' - 1)] 
        ypoints = map (\n -> (g n, n)) [(y+1),(y+1+stepSize)..(y' - 1)]
        g n = (n - b) / m
        f n = (n * m) + b
        b = y - (x * m)
        m = (y' - y) / (x' - x)
getLinesPrecise _ _ = []

--Take a graph and a window size, and create an internal graph which has a scaled set to the window size, and
--intermediate points to draw.
toInternalPure ::(Fractional a, Enum a) => Graph a a -> Window Integer -> InternalGraph a a a
toInternalPure graph window = InternalGraph {
        imaxX = (maxX graph),
        iminX = (minX graph),
        imaxY = (maxY graph),
        iminY = (minY graph),
        ititle = (title graph),
        baseSet = (dataSet graph),
        scaledSet = scaledSet,
        lineSet = getLinesPrecise 1 scaledSet,
        window = window
    }
    where 
        h = fromIntegral $ height window
        w = fromIntegral $ width window
        scaledSet = normalizeSet h w (minX graph) (maxX graph) (minY graph) (maxY graph) (dataSet graph)

-- take a graph and scale it to be fit to the screen
toInternal :: (Fractional a, Enum a) => Graph a a -> IO (Maybe (InternalGraph a a a))
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
                        
toInternalIntHelp :: (Integral a, Fractional b, Ord b, Enum b) =>
     Graph a a -> IO (Maybe (InternalGraph b b b))
toInternalIntHelp g = toInternal $ editGraph (map (\(x,y) -> (fromIntegral x, fromIntegral y))) g

--Take an integer graph and scale it to fit to the screen
toInternalInt :: (Integral a, RealFrac b, Enum b) => Graph a a -> IO (Maybe (InternalGraph a a b))
toInternalInt g = do
                    maybeInt <- internal
                    case maybeInt of
                        Nothing -> return Nothing
                        Just int -> return $ Just (InternalGraph {
                                imaxX = round (imaxX int),
                                iminX = round (iminX int),
                                imaxY = round (imaxY int),
                                iminY = round (iminY int),
                                ititle = (ititle int),
                                baseSet = map (\(x,y) -> (round x, round y)) (baseSet int),
                                scaledSet = map (\(x,y) -> (round x, round y)) (scaledSet int),
                                lineSet = map (\(xs,c) -> (map (\(x,y) -> (round x, round y)) xs, c)) (lineSet int),
                                window = (window int)
                            })
    where internal = (toInternalIntHelp g)