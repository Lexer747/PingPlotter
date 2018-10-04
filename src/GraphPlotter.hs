module GraphPlotter where

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
--char will be the at the point (x+len, y) if X or (x,y-len) if Y
addStringToPlot :: Axis -> (Integer,Integer) -> String -> Plot -> Plot
addStringToPlot X (x,y) (c:cs) plot = addStringToPlot X (x+1,y) cs (addPointToPlot (x,y) c plot)
addStringToPlot Y (x,y) (c:cs) plot = addStringToPlot Y (x,y-1) cs (addPointToPlot (x,y) c plot)
addStringToPlot _ _     []     plot = plot


-- add an individual axis to the plot
addAxisToPlot :: (Show a) => Axis -> [(Integer, a)] -> Plot -> Plot
addAxisToPlot X axis plot = foldr (\(point,str) p -> addStringToPlot X point (show str) p)  plot points
    where points = map (\(x, a) -> ((x,0), a)) axis
addAxisToPlot Y axis plot = foldr (\(point,str) p -> addStringToPlot Y point (show str) p)  plot points
    where points = map (\(y, a) -> ((0,y), a)) axis
    
-- add the numbers from the axis in the graph data to the plot
addAxesToPlot :: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
addAxesToPlot plot g = addAxisToPlot Y (yAxisData g) (addAxisToPlot X (xAxisData g) plot)
      
-- add the blank '-' and '|' characters to each side of plot
addBlankAxesToPlot :: Plot -> InternalGraph a b -> Plot
addBlankAxesToPlot plot g = addPointToPlot (0,0) '+' base1
    where base1 = foldr (\x p -> addPointToPlot x '|' p) base0 (zip [0,0..] [0..h])
          base0 = foldr (\x p -> addPointToPlot x '-' p) plot (zip [0..w] [0,0..])
          w = width $ window g
          h = height $ window g
          
-- add the names of the axes to the graph, x horizontally to the end of the axis, and y vertically to the top of axis
addAxesNameToPlot:: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
addAxesNameToPlot plot g = addStringToPlot Y (yGap, (height $ window g)) (yAxis $ graph g) base0
    where base0 = addStringToPlot X ((width $ window g) - (fromIntegral $ length $ xAxis $ graph g),1) (xAxis $ graph g) plot
          yGap = fromIntegral $ length $ yAxis $ graph g
    
    
-- fold over every point in the graph and add them to a given plot
addGraphToPlot :: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
addGraphToPlot plot g = foldr (\x p -> addPointToPlot x 'X' p) plot (plottingSet g)

-- fold over the lineSet in the graph and add every point to the plot
addGradientToPlot :: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
addGradientToPlot plot g = foldr (\(xs,c) p -> foldr (\x p -> addPointToPlot x c p) p xs) plot (lineSet g)

addTitleToPlot :: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
addTitleToPlot plot g = addStringToPlot X mid (title $ graph g) plot
    where mid = (((width $ window g) `div` 2) - (fromIntegral $ length (title $ graph g) `div` 2), (height $ window g) - 1)

-- combine all the add functions into one function to fill and blank plot with a graph
populateGraph :: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
populateGraph p g = addTitleToPlot (addAxesNameToPlot (addAxesToPlot (addGraphToPlot (addGradientToPlot (addBlankAxesToPlot p g) g) g) g) g) g

----------------------------------------------------------------

-- plot a graph
graphToPlot :: Graph Integer Integer -> IO (Maybe Plot)
graphToPlot g = do
                    maybeInt <- internal --convert to internal representation
                                         --finding the scaled points to the screen
                                         --and inbetween points
                    case maybeInt of
                        Nothing -> return Nothing
                        Just int -> return $ Just $ populateGraph plot int --add all the points to the plot
                            where plot = initPlot (window int) --initalize an empty plot
    where internal = toInternal g
    
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


