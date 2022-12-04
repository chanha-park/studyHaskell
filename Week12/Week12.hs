{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module Week12 where

import Control.Applicative

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

first :: (a -> b) -> (a, c) -> (b, c)
first f = (,) <$> (f . fst) <*> snd

instance Functor Parser where
  fmap f x = Parser $ fmap (first f) . runParser x

instance Applicative Parser where
  pure x = Parser $ \y -> Just (x, y)
  (<*>) (Parser fn1) p2 = Parser fn'
    where
      fn' = \str -> case fn1 str of
        Nothing -> Nothing
        Just (p', str') -> runParser (fmap p' p2) $ str'

instance Alternative Parser where
  empty = Parser . const $ Nothing
  (<|>) (Parser p1) (Parser p2) = Parser $ liftA2 (<|>) p1 p2

-- class Applicative m => Monad m where
--   return :: a -> m a
--   return = pure
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   m1 >> m2 = m1 Week12.>>= \_ -> m2

-- instance Monad Maybe where
--   return = Just
--   Nothing >>= _ = Nothing
--   Just x >>= k = k x

-- instance Monad [] where
--   return x = [x]
--   xs >>= k = concat (fmap k xs)

check :: Int -> Maybe Int
check n
  | n < 10 = Just n
  | otherwise = Nothing

halve :: Int -> Maybe Int
halve n
  | even n = Just $ div n 2
  | otherwise = Nothing

ex01 :: Maybe Int
ex01 = return 7 >>= check >>= halve

ex02 :: Maybe Int
ex02 = return 12 >>= check >>= halve

ex03 :: Maybe Int
ex03 = return 12 >>= halve >>= check

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x + 1, x + 2]

ex04 :: [Int]
ex04 = [10, 20, 30] >>= addOneOrTwo

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma : mas) = ma >>= \a -> Week12.sequence mas >>= \as -> return (a : as)

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = Week12.sequence (replicate n m)

-- replicateM = ((.) Week12.sequence) . replicate

-- parseFile :: Parser [[Int]]
-- parseFile = many parseLine

-- parseLine :: Parser [Int]
-- parseLine = parseInt >>= \i -> replicateM i parseInt

-- parseInt :: Parser Int
-- parseInt = undefined
