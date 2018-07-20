module InternalGraph where
    
import System.Console.Terminal.Size

import Utils

data Graph a b = Graph {
        maxX :: a,
        minX :: a,
        maxY :: b,
        minY :: b,
        title :: String,
        dataSet :: [(a,b)]
    }
        deriving (Show)
            
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
 
normalize :: Fractional a => a -> a -> a -> a -> a -> a
normalize cur min max newMin newMax = (((newMax - newMin) * (cur - min)) / (max - min)) + newMin

normalizeInt :: Integral a => a -> a -> a -> a -> a -> a
normalizeInt cur min max newMin newMax = (((newMax - newMin) * (cur - min)) `div` (max - min)) + newMin

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
        
normalizeSet :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
normalizeSet h w x x' y y' xs = map (\(n,m)-> ((normalizeInt n x x' 0 w), (normalizeInt m y y' 0 h))) xs
        
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
        getPoints = getLinesPrecise toFloat
        toFloat = map (\(x,y) -> (fromIntegral x, fromIntegral y)) xs

--

getLinesPrecise :: [(Float,Float)] -> [([(Float,Float)], Float)]
getLinesPrecise ((x,y):(x',y'):xs) = [(points, m)] ++ (getLinesPrecise ((x',y'):xs))
    where
        points = xpoints ++ ypoints
        xpoints = map (\n -> (n, f n)) [(x+1)..(x' - 1)] 
        ypoints = map (\n -> (g n, n)) [(y+1)..(y' - 1)]
        g n = (n - b) / m
        f n = (n * m) + b
        b = y - (x * m)
        m = (y' - y) / (x' - x)
getLinesPrecise (x:xs) = []
getLinesPrecise [] = []