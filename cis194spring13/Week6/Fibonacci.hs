{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

-- Exercise 1
fib :: Integer -> Integer
fib a
  | a <= 1 = a
  | otherwise = fib (a - 1) + fib (a - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = fibs2' 0 1
  where fibs2' a b = a : fibs2' b (a + b)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Stream a) where
  show a = show (take 30 (streamToList a))

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat = \a -> Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn = \(Cons a b) -> Cons (fn a) (streamMap fn b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn = \a -> Cons a (streamFromSeed fn (fn a))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a b) c = Cons a (interleaveStreams c b)

ruler :: Stream Integer
ruler = fn 0
  where fn a = interleaveStreams (streamRepeat a) (fn (a + 1))

-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger = streamFromSeed (*0)
  negate = streamMap (negate)
  (+) (Cons a b) (Cons c d) = Cons (a + c) (b + d)
  (*) (Cons a b) (Cons c d) = Cons (a * c) (streamMap (* a) d + (b * (Cons c d)))
  abs = streamMap abs
  signum = streamMap signum

division :: (Stream Integer) -> (Stream Integer) -> (Stream Integer)
division (Cons a b) (Cons c d) = q
  where q = Cons (div a c) (streamMap (`div` c) (b - q * d))

fibs3 :: Stream Integer
fibs3 = division x (1 - x - x * x)

-- Exercise 7
data Matrix a = Matrix a a a a

instance Num (Matrix Integer) where
  fromInteger n = Matrix n n n n
  negate (Matrix a b c d) = Matrix (negate a) (negate b) (negate c) (negate d)
  abs (Matrix a b c d) = Matrix (abs a) (abs b) (abs c) (abs d)
  signum (Matrix a b c d) = Matrix (signum a) (signum b) (signum c) (signum d)
  (+) (Matrix a b c d) (Matrix a' b' c' d') = Matrix (a + a') (b + b') (c + c') (d + d')
  (*) (Matrix a b c d) (Matrix a' b' c' d') = Matrix (a * a' + b * c') (a * b' + b * d') (c * a' + d * c') (c * b' + d * d')

fiboMat :: Matrix Integer
fiboMat = Matrix 1 1 1 0

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = (\(Matrix _ b _ _) -> b) (fiboMat ^ n)
