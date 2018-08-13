module GraphPlotter  where

import System.Console.Terminal.Size
import Data.List
import Data.Array
import Data.Maybe (fromJust)
import InternalGraph
import GraphTypes
import Utils

type Plot = Array Integer (Array Integer Char)

-- chars used for the graph --
intrapunct = '·' --a centralized point
blank = ' ' -- a blank char
------------------------------

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
    
--adds a string to a plot, the first char starts at the specified point (x,y), and the last
--char will be the at the point (x+len, y)
addStringToPlot :: (Integer,Integer) -> String -> Plot -> Plot
addStringToPlot (x,y) (c:cs) plot = addStringToPlot (x+1,y) cs (addPointToPlot (x,y) c plot)
addStringToPlot _     []     plot = plot

-- add an individual axis to the plot
addAxisToPlot :: Axis -> [Integer] -> Plot -> Plot
addAxisToPlot X axis plot = foldr (\(str,point) p -> addStringToPlot point (show str) p)  plot points
    where points = zip axis (zip [3,(xaxisGap+3)..] [0,0..])
addAxisToPlot Y axis plot = foldr (\(str,point) p -> addStringToPlot point (show str) p)  plot points
    where points = zip axis (zip [0,0..] [3,(yaxisGap+3)..])
    
-- add the numbers from the axis in the graph data to the plot
addAxesToPlot :: Plot -> InternalGraph Integer Integer a -> Plot
addAxesToPlot plot graph = addAxisToPlot Y (yAxis graph) (addAxisToPlot X (xAxis graph) plot)
      
-- add the blank '-' and '|' characters to each side of plot
addBlankAxesToPlot :: Plot -> InternalGraph Integer Integer a -> Plot
addBlankAxesToPlot plot graph = addPointToPlot (0,0) '+' base1
    where base1 = foldr (\x p -> addPointToPlot x '|' p) base0 (zip [0,0..] [0..h])
          base0 = foldr (\x p -> addPointToPlot x '-' p) plot (zip [0..w] [0,0..])
          w = width $ window graph
          h = height $ window graph
    
-- fold over every point in the graph and add them to a given plot
addGraphToPlot :: Plot -> InternalGraph Integer Integer a -> Plot
addGraphToPlot plot graph = foldr (\x p -> addPointToPlot x 'X' p) plot (scaledSet graph)

-- fold over the lineSet in the graph and add every point to the plot
addGradientToPlot :: Plot -> InternalGraph Integer Integer Char -> Plot
addGradientToPlot plot graph = foldr (\(xs,c) p -> foldr (\x p -> addPointToPlot x c p) p xs) plot (lineSet graph)

addTitleToPlot :: Plot -> InternalGraph Integer Integer Char -> Plot
addTitleToPlot plot graph = addStringToPlot mid (ititle graph) plot
    where mid = (((width $ window graph) `div` 2) - (fromIntegral $ length (ititle graph) `div` 2), (height $ window graph) - 1)

-- combine all the add functions into one function to fill and blank plot with a graph
populateGraph :: Plot -> InternalGraph Integer Integer Char -> Plot
populateGraph p g = addTitleToPlot (addAxesToPlot (addGraphToPlot (addGradientToPlot (addBlankAxesToPlot p g) g) g) g) g

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
                        xAxis = (xAxis g),
                        yAxis = (yAxis g),
                        scaledSet = (scaledSet g),
                        lineSet = map (\(x,c) -> (x, gradient c)) (lineSet g),
                        window = (window g)
                   }
    
gradient :: Double -> Char
gradient _ = intrapunct -- TODO
{-
doesn't work great -_-
gradient x | x >= 10 = '|'
gradient x | x <= (-10) = '|'
gradient x | x >= 2 = '/'
gradient x | x <= (-2) = '\\'
gradient x = '-'
-}



    


-- plot a graph
graphToPlot :: Graph Integer Integer -> IO (Maybe Plot)
graphToPlot g = do
                    maybeInt <- internal --convert to internal representation
                                         --finding the scaled points to the screen
                                         --and inbetween points
                    case maybeInt of
                        Nothing -> return Nothing
                        Just int -> return $ Just $ populateGraph plot graph --add all the points to the plot
                            where graph = gradientToChar int --convert the gradients to chars
                                  plot = initPlot (window graph) --initalize an empty plot
    where internal = toInternalInt g
    
unsafe_graphToPlot :: Graph Integer Integer -> IO (Plot)
unsafe_graphToPlot g = do
                            maybeG <- graphToPlot g
                            return $ fromJust maybeG
    
graphPrint :: Graph Integer Integer -> IO ()
graphPrint g = do
                    maybePlot <- graphToPlot g
                    case maybePlot of 
                        Nothing -> putStrLn "Failed - window size probably failed"
                        Just plot -> putStrLn $ plotToPrintString plot


