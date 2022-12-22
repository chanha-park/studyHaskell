{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

module Golf where

-- import Data.List (sort)

skips :: [a] -> [[a]]
skips [] = []
skips x = x : skips (tail x)
-- skips (x : xs) = (x : xs) : skips xs

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : ds)
  | (b > a && b > c) = b : localMaxima (b : c : ds)
  | otherwise = localMaxima (b : c : ds)
localMaxima _ = []

-- histogram :: [Integer] -> String
