module Utils (
    unique, right, left, divide, (//), getMinMax
) where

import Data.List

--removes all duplicate elements from a list, ands sorts it
--[1,2,2] -> [1,2]
--[1,2,1] -> [1,2]
unique :: Ord a => [a] -> [a]
unique x = rm $ sort x

--removes elements which are next to each other and the same, until only 1 is left
--[1,2,2] -> [1,2]
--[1,2,1] -> [1,2,1]
rm :: Eq a => [a] -> [a]
rm [] = []
rm [x] = [x]
rm (x:y:xs) 
    | x == y = rm (y:xs)
    | otherwise = x:(rm (y:xs))

right (_,x) = x
left (x,_) = x

--ceiling integer division
divide :: Int -> Int -> Int
divide x y = ceiling $ (fromIntegral x) / (fromIntegral y)
(//) = divide

-- takes a list an returns a tuple containing the smallest and largest element
-- o(n) complexity
getMinMax :: Ord a => [a] -> (a,a)
getMinMax (x:xs) = getMinMax_ xs (x,x)

getMinMax_ :: Ord a => [a] -> (a,a) -> (a,a)
getMinMax_ (x:xs) (a,b) | x < a = getMinMax_ xs (x,b)
getMinMax_ (x:xs) (a,b) | x > b = getMinMax_ xs (a,x)
getMinMax_ (_:xs) acc           = getMinMax_  xs acc
getMinMax_ [] acc               = acc