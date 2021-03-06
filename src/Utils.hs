module Utils
    (
    unique,
    right,
    left,
    divide,
    (\\),
    getMinMax,
    mapS,
    mapA,
    symmetric,
    asymmetric,
    removeLast,
    wordsWhen,
    mySort,
    takeTail
    ) where

import Data.List (sort)

--removes all duplicate elements from a list, and sorts it
--[1,2,2] -> [1,2]
--[2,1,1] -> [1,2]
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

-- taking things out of tuples
right :: (a,b) -> b
right (_,x) = x

left :: (a,b) -> a
left (x,_)  = x

--ceiling integer division
divide :: Integral a => a -> a -> a
divide x y = ceiling $ (fromIntegral x) / (fromIntegral y)

(\\) :: Integral a => a -> a -> a
(\\) = divide --infix

-- takes a list an returns a tuple containing the smallest and largest element
-- o(n) complexity
getMinMax :: Ord a => [a] -> (a,a)
getMinMax (x:xs) = getMinMax_ xs (x,x)
getMinMax [] = error "empty list passed to getMinMax"

getMinMax_ :: Ord a => [a] -> (a,a) -> (a,a)
getMinMax_ (x:xs) (a,b) | x < a = getMinMax_ xs (x,b)
getMinMax_ (x:xs) (a,b) | x > b = getMinMax_ xs (a,x)
getMinMax_ (_:xs) acc           = getMinMax_  xs acc
getMinMax_ [] acc               = acc

--given a function, apply it to a tuple which is of the same type
symmetric :: (a -> b) -> (a,a) -> (b,b)
symmetric f (x,y) = (f x,f y)

-- given 2 functions, apply each one to half the tuple each
asymmetric :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
asymmetric f w (x,y)= (f x,w y)

--mapSymmetric, given a function map it symmetrically to both sides of a tupled list
mapS :: (a -> b) -> [(a, a)] -> [(b, b)]
mapS f xs = map (symmetric f) xs

--mapAsymmetric, given two different functions map each one to its own half of a tupled list
mapA :: (a -> c) -> (b -> d) -> [(a, b)] -> [(c, d)]
mapA f w xs = map (asymmetric f w) xs

-- O(n) removal of the last char
removeLast :: String -> String
removeLast (_:[])   = []
removeLast (x:xs)   = x:(removeLast xs)
removeLast []       = []

-- see {Data.List (words)}, but use a predicate instead spaces
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

--sort given a function instead of on the Ord class
-- since some types cannot have their Ord definition overwritten easily
mySort :: (a -> a -> Bool) -> [a] -> [a]
mySort _ []     = []
mySort f (x:xs) = let small = mySort f [a | a <- xs, f x a]
                      big   = mySort f [a | a <- xs, not $ f x a]
                  in small ++ [x] ++ big

-- like take, but from the end of the list rather than the start
takeTail :: Int -> [a] -> [a]
takeTail i xs = reverse $ take i $ reverse xs