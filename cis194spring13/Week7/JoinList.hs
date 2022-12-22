{-# OPTIONS_GHC -Wall -Werror -Wextra #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JoinList where

import           Buffer
import           Scrabble
import           Sized

-- Simplified example
-- data JoinListBasic a = Empty
--                      | Single a
--                      | Append (JoinListBasic a) (JoinListBasic a)

-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty        = []
-- jlbToList (Single a)   = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)


-- newtype Product a = Product a
--     deriving (Eq, Ord, Num, Show)

-- getProduct :: Product a -> a
-- getProduct (Product a) = a

-- instance Num a => Monoid (Product a) where
--   mempty = Product 1
--   mappend = (<>)

-- instance Num a => Semigroup (Product a) where
--   (<>) = (*)

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (mappend (tag x) (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n _ | n < 0 = Nothing
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ n (Append _ x y)
  | n < sizeX = indexJ n x
  | otherwise = indexJ (n - sizeX) y
  where sizeX = (getSize . size . tag) x

-- indexJ n x = (!!?) (jlToList x) n

-- (!!?) :: [a] -> Int -> Maybe a
-- (!!?) [] _ = Nothing
-- (!!?) _ n | n < 0 = Nothing
-- (!!?) (x : _) 0 = Just x
-- (!!?) (_ : xs) n = (!!?) xs (n - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x | n < 0 = x
dropJ _ Empty = Empty
dropJ 0 (Single m x) = Single m x
dropJ _ (Single _ _) = Empty
dropJ n (Append _ x y)
  | n < sizeX = (dropJ n x) +++ y
  | otherwise = dropJ (n - sizeX) y
  where sizeX = (getSize . size . tag) x

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n < 0 = Empty
takeJ _ Empty = Empty
takeJ 0 (Single _ _) = Empty
takeJ _ (Single m x) = Single m x
takeJ n (Append _ x y)
  | n < sizeX = takeJ n x
  | otherwise = x +++ takeJ (n - sizeX) y
  where sizeX = (getSize . size . tag) x

instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty

instance Monoid m => Semigroup (JoinList m a) where
  (<>) = (+++)

scoreLine :: String -> JoinList Score String
scoreLine = \x -> Single (scoreString x) x

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = mconcat . fmap (\x -> Single (scoreString x, Size 1) x) . lines
  line = indexJ
  numLines = getSize . snd . tag
  value = getScore . fst . tag
  replaceLine n str x = takeJ n x +++ fromString str +++ dropJ (n + 1) x
