{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs

gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 xs = filter (\x -> x > 100) xs

greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 xs = filter (> 100) xs

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)

foo' :: (b -> c) -> (a -> b) -> a -> c
foo' f g x = f (g x)

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

bar :: Int -> Int -> Int
bar x y = 2 * x + y

bar' :: Int -> (Int -> Int)
bar' x y = 2 * x + y

foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x : xs)
  | x > 3 = (7 * x + 2) + foobar xs
  | otherwise = foobar xs


foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7 * x + 2) . filter (> 3)
