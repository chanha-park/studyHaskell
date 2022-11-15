{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (+ (-2)) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (div n 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> case x of
                                                                _ | even x -> div x 2
                                                                  | otherwise -> 3 * x + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- XXX
-- foldTree :: [a] -> Tree a
-- foldTree [] = Leaf


xor :: [Bool] -> Bool
xor = foldr (/=) False

-- map using fold
-- map' :: (a -> b) -> [a] -> [b]

-- myFoldl using foldr
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a

-- finding primes
-- sieveSundaram :: Integer -> [Integer]

