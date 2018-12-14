module GraphPlotter where

import System.Console.Terminal.Size
import Data.List
import Data.Array
import Data.Maybe (fromJust)
import InternalGraph
import GraphTypes
import Utils

--a plot is a rectangle 2D array of characters
type Plot = Array Integer (Array Integer Char)

-- chars used for the graph --
intrapunct = '·' --a centralized point
blank = ' ' -- a blank char
---------------------------------------------------

--initalize an empty plot
initPlot :: Window Integer -> Plot
initPlot window = array (0,h) [ (i, (array (0,w) [(i,blank) | i <- [0..w]])) | i <- [0..h]]
    where (h,w) = ((height window), (width window))

--convert a plot to a string which concatenates each row with a newline
plotToPrintString :: Plot -> String
plotToPrintString p = removeLast $ unlines $ reverse $ map elems $ elems p

--add a point with a representative char
addPointToPlot :: (Integer, Integer) -> Char -> Plot -> Plot
addPointToPlot (x,y) c plot = plot // [(y, (xrow // [(x,c)]))]
    where xrow = plot ! y

--adds a string to a plot, the first char starts at the specified point (x,y), and the last
--char will be the at the point (x+len, y)
addStringToPlot :: Axis -> (Integer,Integer) -> String -> Plot -> Plot
addStringToPlot _ (x,y) (c:cs) plot = addStringToPlot X (x+1,y) cs (addPointToPlot (x,y) c plot)
addStringToPlot _ _     []     plot = plot
      
-- add the blank '-' and '|' characters to each side of plot
addBlankAxesToPlot :: Plot -> InternalGraph a b -> Plot
addBlankAxesToPlot plot g = addPointToPlot (0,0) '+' base1
    where base1 = foldr (\x p -> addPointToPlot x '|' p) base0 (zip [0,0..] [0..h])
          base0 = foldr (\x p -> addPointToPlot x '-' p) plot (zip [0..w] [0,0..])
          w = width $ window g
          h = height $ window g

-- add the names of the axes to the graph, x horizontally to the end of the axis, and y horizontally to the top of axis
addAxesNameToPlot:: Plot -> InternalGraph a b -> Plot
addAxesNameToPlot plot g = addStringToPlot Y (1, (height $ window g)) (yAxis $ graph g) base0
    where base0 = addStringToPlot X ((width $ window g) - (fromIntegral $ length $ xAxis $ graph g),1) (xAxis $ graph g) plot

-- fold over every point in the graph and add them to a given plot
addGraphToPlot :: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
addGraphToPlot plot g = foldr (\x p -> addPointToPlot x 'X' p) plot (plottingSet g)

-- fold over the lineSet in the graph and add every point to the plot
addGradientToPlot :: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
addGradientToPlot plot g = foldr (\(xs,c) p -> foldr (\x p -> addPointToPlot x c p) p xs) plot (lineSet g)

-- centrally put the title on the plot
addTitleToPlot :: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
addTitleToPlot plot g = addStringToPlot X mid (title $ graph g) plot
    where mid = (((width $ window g) `div` 2) - (fromIntegral $ length (title $ graph g) `div` 2), (height $ window g) - 2)

-----------------------------------------------------------------

-- combine all the add functions into one function to fill and blank plot with a graph
-- note: the order in which the functions are called is the order in which they are painted
-- so the first one called will be painted over by other calls
populateGraph :: (Show a, Show b) => Plot -> InternalGraph a b -> Plot
populateGraph p g = addTitleToPlot (addAxesNameToPlot (addGraphToPlot (addGradientToPlot (addBlankAxesToPlot p g) g) g) g) g

----------------------------------------------------------------

-- add an individual axis to the plot
addAxisToPlot :: (IOShow a) => Axis -> [(Integer, a)] -> Plot -> IO Plot
addAxisToPlot X ((x,a):xs) plot = do 
                str <- ioShow a
                let p = addStringToPlot X (x,0) str plot
                addAxisToPlot X xs p
addAxisToPlot Y ((y,a):ys) plot = do 
                str <- ioShow a
                let p = addStringToPlot Y (0,y) str plot
                addAxisToPlot Y ys p
addAxisToPlot _ _ plot = return plot
    
-- add the numbers from the axis in the graph data to the plot
addAxesToPlot :: (IOShow a, IOShow b) => Plot -> InternalGraph a b -> IO Plot
addAxesToPlot plot g = do 
    p <- addAxisToPlot Y (yAxisData g) plot
    addAxisToPlot X (xAxisData g) p

----------------------------------------------------------------


-- plot a graph
graphToPlot :: (Show a, Show b, IOShow a, IOShow b, RealFrac x, Enum x, Ord x) =>
    (a -> x) -> (b -> x) -> Graph a b -> IO (Maybe Plot)
graphToPlot convertX convertY g = do
        maybeInt <- internal --convert to internal representation
                             --finding the scaled points to the screen
                             --and in between points
        case maybeInt of
            Nothing  -> return Nothing
            Just int -> do
                   let plot = populateGraph (initPlot (window int)) int --add all the points to the plot
                   p <- addAxesToPlot plot int 
                   return $ Just p
    where internal = toInternal convertX convertY g
    
unsafe_graphToPlot :: (Show a, Show b, IOShow a, IOShow b, RealFrac x, Enum x, Ord x) =>
    (a -> x) -> (b -> x) ->  Graph a b -> IO (Plot)
unsafe_graphToPlot cX cY g = do
    maybeG <- graphToPlot cX cY g
    return $ fromJust maybeG

graphPrint :: (Show a, Show b, IOShow a, IOShow b, RealFrac x, Enum x, Ord x) =>
    (a -> x) -> (b -> x) -> Graph a b -> IO ()
graphPrint cX cY g = do
    maybePlot <- graphToPlot cX cY g
    case maybePlot of 
        Nothing -> putStrLn "graphPrint Failed - cause: window size probably failed"
        Just plot -> putStr $ plotToPrintString plot