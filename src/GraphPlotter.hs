module GraphPlotter where

import System.Console.Terminal.Size

import Data.List

import InternalGraph
import Constants
import Utils

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