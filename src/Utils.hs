module Utils where

import Data.List


unique :: Ord a => [a] -> [a]
unique x = rm $ sort x

rm :: Eq a => [a] -> [a]
rm [] = []
rm [x] = [x]
rm (x:y:xs) 
    | x == y = rm (y:xs)
    | otherwise = x:(rm (y:xs))

right (_,x) = x
left (x,_) = x

divide :: Int -> Int -> Int
divide x y = ceiling $ (fromIntegral x) / (fromIntegral y)

(//) = divide