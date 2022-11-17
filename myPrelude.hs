{-# OPTIONS_GHC -Wall -Werror -Wextra #-}
-- custom Prelude Function implementation
-- foldr', foldl' ???

myFoldr :: (a -> b -> b ) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr fn acc (x : xs) = fn x (myFoldr fn acc xs)

myFoldl :: (b -> a -> b ) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl fn acc (x : xs) = myFoldl fn (fn acc x) xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap fn (x : xs) = (fn x) : (myMap fn xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter fn (x : xs)
  | fn x = x : (myFilter fn xs)
  | otherwise = myFilter fn xs

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myLength :: [a] -> Int
myLength = myFoldr (\_ y -> 1 + y) 0

mySum :: Num a => [a] -> a
mySum = myFoldr (+) 0

myProduct :: Num a => [a] -> a
myProduct = myFoldr (*) 1

myAnd :: [Bool] -> Bool
myAnd = myFoldr (&&) True

myOr :: [Bool] -> Bool
myOr = myFoldr (||) False

myXor :: [Bool] -> Bool
myXor = myFoldr (/=) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny fn = myOr . map fn

myAll :: (a -> Bool) -> [a] -> Bool
myAll fn = myAnd . map fn

-- naive qsort implementation
myQsort :: Ord a => [a] -> [a]
myQsort [] = []
myQsort (x : xs) = (myQsort [y | y <- xs, y < x]) ++ [x] ++ myQsort [z | z <- xs, z >= x]
