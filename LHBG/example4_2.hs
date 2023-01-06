{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

import Prelude (
    Bool (False, True),
    Int,
    otherwise,
    (-),
    (<=),
    (==),
 )

replicate :: Int -> a -> [a]
replicate n x
    | n <= 0 = []
    | otherwise = x : replicate (n - 1) x

even :: Int -> Bool
even n
    | n == 0 = True
    | otherwise = odd (n - 1)

odd :: Int -> Bool
odd n
    | n == 0 = False
    | otherwise = even (n - 1)
