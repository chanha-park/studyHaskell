{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

-- Exercise 1
fib :: Integer -> Integer
fib x
  | x <= 1 = x
  | otherwise = fib (x - 1) + fib (x - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = fibs2' 0 1
  where fibs2' x y = x : fibs2' y (x + y)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

instance Show a => Show (Stream a) where
  show x = show (take 30 (streamToList x))

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat = \x -> Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn = \(Cons x y) -> Cons (fn x) (streamMap fn y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn = \x -> Cons x (streamFromSeed fn (fn x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x y) z = Cons x (interleaveStreams z y)

ruler :: Stream Integer
ruler = fn 0
  where fn x = interleaveStreams (streamRepeat x) (fn (x + 1))
