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

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf
  where
    insertTree :: a -> Tree a -> Tree a
    insertTree x Leaf = Node 0 Leaf x Leaf
    insertTree x (Node h l val r)
      | getHeight l < getHeight r = Node h (insertTree x l) val r
      | getHeight l > getHeight r = Node h l val (insertTree x r)
      | otherwise = Node (1 + getHeight l') l' val r
      where l' = insertTree x l
    getHeight :: Tree a -> Integer
    getHeight Leaf = -1
    getHeight (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldr (/=) False

-- map using fold
map' :: (a -> b) -> [a] -> [b]
map' fn = foldr (\x xs -> fn x : xs) []

-- myFoldl using foldr
myFoldl :: forall a b. (a -> b -> a) -> a -> [b] -> a
myFoldl fn acc xs = (foldr (\x g -> \y -> g (fn y x)) id xs) acc
-- myFoldl fn acc xs = (foldr (\x g y -> g (fn y x)) id xs) acc
-- myFoldl fn acc xs = (foldr fn' id xs) acc
--   where
--     fn' :: b -> (a -> a) -> (a -> a)
--     fn' x g = \y -> g (fn y x)


-- finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram  n = map (\x -> 2 * x + 1) (filter (\x -> (notElem x arrRm)) [1..n])
  where
    arrRm :: [Integer]
    arrRm = [i + j + 2 * i * j | i <- [1..n], j <- [i..n], i + j + 2 * i * j <= n]
