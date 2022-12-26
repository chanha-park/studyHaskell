{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module Sieve where

import Data.Map

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve [] = []
    sieve (p : xs) = p : sieve [x | x <- xs, mod x p > 0]

primes' :: [Integer]
primes' = 2 : [x | x <- [3 ..], isprime x]
  where
    isprime x = all (\p -> mod x p > 0) (factorsToTry x)
    factorsToTry x = Prelude.takeWhile (\p -> p * p <= x) primes'

-- priority queue can be used instead of map
primes'' :: [Integer]
primes'' = sieve [2 ..]
  where
    sieve xs = sieve' xs Data.Map.empty
    sieve' [] _ = []
    sieve' (x : xs) table = case Data.Map.lookup x table of
      Nothing -> x : sieve' xs (Data.Map.insert (x * x) [x] table)
      Just facts -> sieve' xs (Prelude.foldl reinsert (delete x table) facts)
        where
          reinsert table' prime = insertWith (++) (x + prime) [prime] table'

primes''' :: [Integer]
primes''' = 2 : ([3..] `minus` composites)
  where
    composites = union' [multiples p | p <- primes''']
    multiples n = Prelude.map (n *) [n..]
    [] `minus` _ = []
    _ `minus` [] = []
    (x : xs) `minus` (y : ys)
      | x < y = x : (xs `minus` (y : ys))
      | x > y = (x : xs) `minus` ys
      | otherwise = xs `minus` ys
    union' = Prelude.foldr merge []
      where
        merge [] ys = ys
        merge (x : xs) ys = x : merge' xs ys
        merge' [] ys = ys
        merge' xs [] = xs
        merge' (x : xs) (y : ys)
          | x < y = x : merge' xs (y : ys)
          | x > y = y : merge' (x : xs) ys
          | otherwise = x : merge' xs ys

count :: Int
count = 10000

get :: [a] -> a
get xs = xs Prelude.!! count

main :: IO ()
main = putStrLn "Start" >> (print . get $ primes) >> (print . get $ primes') >> (print . get $ primes'') >> (print . get $ primes''')
