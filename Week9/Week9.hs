{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

module Week9 where

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Week9.Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap h (Just a) = Just (h a)

instance Week9.Functor [] where
  fmap _ []       = []
  fmap f (x : xs) = f x : Week9.fmap f xs

instance Week9.Functor IO where
  fmap f ioa = ioa >>= (return . f)
  -- fmap f ioa = ioa >>= (\a -> return (f a))

instance Week9.Functor ((->) e) where
  fmap = (.)

