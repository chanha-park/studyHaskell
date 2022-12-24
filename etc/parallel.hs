{-# OPTIONS_GHC -Wall -Wextra -Werror -threaded #-}

import Control.Monad
import Control.Parallel
import Text.Printf

cutoff :: Int
cutoff = 35

fib' :: Int -> Integer
fib' 0 = 0
fib' 1 = 1
fib' x = fib' (x - 1) + fib' (x - 2)

fib :: Int -> Integer
fib n
  | n < cutoff = fib' n
  | otherwise = r `par` (l `pseq` l + r)
  where
    l = fib (n - 1)
    r = fib (n - 2)

main :: IO ()
main = forM_ [0 .. 45] $ \k -> printf "n=%d => %d\n" k (fib k)
