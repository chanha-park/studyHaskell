{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

f1 :: a -> a -> a
f1 x _ = x

f2 :: a -> a
f2 x = x

-- f3 :: a -> b

f4 :: [a] -> [a]
f4 _ = []

f5 :: (b -> c) -> (a -> b) -> (a -> c)
f5 x y = x . y

f6 :: (a -> a) -> a -> a
f6 p q = p q

data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

data Foo' = F' Int | G' Char
    deriving (Eq, Ord, Show)

class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

sumL :: Listable a => a -> Int
sumL x = sum (toList x)

foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y

instance (Listable a, Listable b) => Listable (a, b) where
  toList (x, y) = toList x ++ toList y
