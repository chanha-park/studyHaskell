{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

module JoinList where

import           Sized

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
indexJ n (Append x y z)
  | n < getSize (size x) = indexJ n y
  | otherwise = indexJ (n - getSize ( size x)) z
-- indexJ n x = (!!?) (jlToList x) n


(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _ = Nothing
(!!?) _ n | n < 0 = Nothing
(!!?) (x : _) 0 = Just x
(!!?) (_ : xs) n = (!!?) xs (n - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
