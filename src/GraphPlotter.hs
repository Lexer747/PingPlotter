module GraphPlotter  where

import System.Console.Terminal.Size
import Data.List
import Data.Array
import InternalGraph
import GraphTypes
import Utils

type Plot = Array Integer (Array Integer Char)

blank = '.'

--initalize an empty plot
initPlot :: Window Integer -> Plot
initPlot window = array (0,h) [ (i, (array (0,w) [(i,blank) | i <- [0..w]])) | i <- [0..h]]
    where (h,w) = ((height window), (width window))

--convert a plot to a string which concatenates each row with a newline
plotToPrintString :: Plot -> String
plotToPrintString p = unlines $ reverse $ map elems $ elems p

--add a point with a representative char 
addPointToPlot :: (Integer, Integer) -> Char -> Plot -> Plot
addPointToPlot (x,y) c plot = plot // [(y, (xrow // [(x,c)]))]
    where xrow = plot ! y

    
-- fold over every point in the graph and add them to a given plot
addGraphToPlot :: Plot -> InternalGraph Integer Integer Char -> Plot
addGraphToPlot plot graph = foldr (\x p -> addPointToPlot x 'X' p) plot (scaledSet graph)

-- map over the gradient in the line set converting gradients to  char to represent 
-- the gradient we have replaced
gradientToChar :: InternalGraph Integer Integer Double -> InternalGraph Integer Integer Char
gradientToChar g = InternalGraph {
                        imaxX = imaxX g,
                        iminX = iminX g,
                        imaxY = imaxY g,
                        iminY = iminY g,
                        ititle = (ititle g),
                        baseSet = (baseSet g),
                        scaledSet = (scaledSet g),
                        lineSet = map (\(x,c) -> (x, gradient c)) (lineSet g),
                        window = (window g)
                   }
    
gradient :: Double -> Char
gradient x | x >= 15 = '|'
gradient x | x <= (-15) = '|'
gradient x | x >= 3 = '/'
gradient x | x <= (-3) = '\\'
gradient x = '-'

-- plot a graph
graphToPlot :: Graph Integer Integer -> IO (Maybe Plot)
graphToPlot g = do
                    maybeInt <- internal --convert to internal representation
                                         --finding the scaled points to the screen
                                         --and inbetween points
                    case maybeInt of
                        Nothing -> return Nothing
                        Just int -> return $ Just $ addGraphToPlot plot graph --add all the points to the plot
                            where graph = gradientToChar int --convert the gradients to chars
                                  plot = initPlot (window graph) --initalize an empty plot
    where internal = toInternalInt g
    
graphPrint :: Graph Integer Integer -> IO ()
graphPrint g = do
                    maybePlot <- graphToPlot g
                    case maybePlot of 
                        Nothing -> putStrLn "Failed - window size probably failed"
                        Just plot -> putStrLn $ plotToPrintString plot


{-
drawGraph :: Graph Int Int -> IO ()
drawGraph g = do
                newG <- fitToScreen $ toInternal g
                case newG of
                    Just (x,win) -> putStr $ (graphToString $ preconfig x) (findScale x win)
                    Nothing -> putStr ""
                

preconfig :: InternalGraph Int Int Float -> InternalGraph Int Int Char
preconfig g = InternalGraph {
        imaxX = imaxX g,
        iminX = iminX g,
        imaxY = imaxY g,
        iminY = iminY g, 
        ititle = ititle g, 
        idataSet = idataSet g,
        ilineSet = newSet
    }
        where newSet = map (\(y,x) -> (y,'.')) (ilineSet g)
        
printDemo = preconfig demoInternal

graphToString :: InternalGraph Int Int Char -> (Int, Int) -> String
graphToString g scale = title ++ (genGraphString xcoord ycoord scale (idataSet g) (ilineSet g)) ++ yaxis
    where
        xcoord = (iminX g, imaxX g)
        ycoord = (iminY g, (imaxY g - 1))
        title = (replicate ((imaxY g) `div` 2) ' ') ++ (ititle g) ++ "\n"
        yaxis = concatMap (\x -> show x) $ [(left ycoord)..(right ycoord)]
        
genGraphString :: (Int,Int) -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> [([(Int,Int)], Char)] -> String
genGraphString (minX, maxX) (minY, maxY) (stepX, stepY) points lines 
        | maxY > minY = row ++ (genGraphString (minX, maxX) (minY, maxY - stepY) (stepX, stepY) points lines)
        | otherwise = ""
    where
        row = genRowString showY stepX (minX,maxX) $ getPoints (maxY - stepY + 1, maxY) points lines
        showY = Just maxY        
 

    
genRowString :: Maybe Int -> Int -> (Int,Int) -> [(Int, Char)] -> String
genRowString (Just x) stepX (minX, maxX) dic = (show x) ++ recursive
    where recursive = genRowString Nothing stepX (minX, maxX) dic
genRowString Nothing _ _ [] = "\n"
genRowString Nothing stepX (minX, maxX) dic
        | (exists range dic) = [unsafeLookup range dic] ++ recursiveRm
        | minX >= maxX      = "\n"
        | otherwise         = " " ++ recursive
            where recursive = genRowString Nothing stepX (minX + stepX, maxX) dic
                  recursiveRm = genRowString Nothing stepX (minX + stepX, maxX) (rmLookup range dic)
                  range = (minX,minX + stepX)

getPoints :: (Int,Int) -> [(Int,Int)] -> [([(Int,Int)], Char)] -> [(Int, Char)]
getPoints (min,max) ps ls = ps' ++ ls'
    where f (n,m) = map (\z -> (z,m)) n
          ps' = f (left $ unzip $ filter (\(_,x) -> x >= min && x <= max) ps, 'X')
          ls' = concatMap f $ map (\(y,c) -> (left $ unzip (filter (\(_,x) -> x >= min && x <= max) y),c)) ls
          
findScale :: InternalGraph Int Int Float -> Window Int -> (Int, Int)
findScale g win = findScale' (h,yr) (w,xr)
    where h = height win
          w = width win
          xr = (imaxX g) - (iminX g)
          yr = (imaxY g) - (iminY g)
          
findScale' :: (Int,Int) -> (Int,Int) -> (Int,Int)
findScale' (h,yr) (w,xr) = (x,y)
    where x = if (yr // h) <= 0 then 1 else (yr // h)
          y = if (xr // w) <= 0 then 1 else (xr // w)
  
exists :: Ord a => (a,a) -> [(a,b)] -> Bool
exists _ [] = False
exists (min,max) ((x,_):xs) 
    | x >= min && x <= max = True
    | otherwise = exists (min,max) xs
               
unsafeLookup :: Ord a => (a,a) -> [(a,b)] -> b
unsafeLookup _ [] = error "unsafeLookup"
unsafeLookup (min,max) ((x,b):xs) 
    | x >= min && x <= max = b
    | otherwise = unsafeLookup (min,max) xs
                        
rmLookup :: Ord a => (a,a) -> [(a,b)] -> [(a,b)]
rmLookup (min,max) ((x,b):xs) 
    | x >= min && x <= max = rmLookup (min,max) xs
    | otherwise = (x,b):(rmLookup (min,max) xs)
rmLookup _ [] = []

p = idataSet printDemo
l = ilineSet printDemo
-}