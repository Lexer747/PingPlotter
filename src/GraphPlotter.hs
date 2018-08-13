module GraphPlotter  where

import System.Console.Terminal.Size
import Data.List
import Data.Array
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


    
-- fold over every point in the graph and add them to a given plot
addGraphToPlot :: Plot -> InternalGraph Integer Integer Char -> Plot
addGraphToPlot plot graph = foldr (\x p -> addPointToPlot x 'X' p) plot (scaledSet graph)

-- fold over the lineSet in the graph and add every point to the plot
addGradientToPlot :: Plot -> InternalGraph Integer Integer Char -> Plot
addGradientToPlot plot graph = foldr (\(xs,c) p -> foldr (\x p -> addPointToPlot x c p) p xs) plot (lineSet graph)

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


addAxisToPlot :: Plot -> InternalGraph Integer Integer Char -> Plot
addAxisToPlot plot graph =                                                                    

-- plot a graph
graphToPlot :: Graph Integer Integer -> IO (Maybe Plot)
graphToPlot g = do
                    maybeInt <- internal --convert to internal representation
                                         --finding the scaled points to the screen
                                         --and inbetween points
                    case maybeInt of
                        Nothing -> return Nothing
                        Just int -> return $ Just $ addGraphToPlot (addGradientToPlot plot graph) graph --add all the points to the plot
                            where graph = gradientToChar int --convert the gradients to chars
                                  plot = initPlot (window graph) --initalize an empty plot
    where internal = toInternalInt g
    
graphPrint :: Graph Integer Integer -> IO ()
graphPrint g = do
                    maybePlot <- graphToPlot g
                    case maybePlot of 
                        Nothing -> putStrLn "Failed - window size probably failed"
                        Just plot -> putStrLn $ plotToPrintString plot


