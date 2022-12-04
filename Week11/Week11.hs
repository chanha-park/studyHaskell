{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

module Week11 where

import Control.Applicative

-- From Week10
type Name = String

data Employee = Employee {name :: Name, phone :: String}
  deriving (Show)

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative [] where
--   pure a = [a]
--   [] <*> _ = []
--   (f : fs) <*> as = fmap f as ++ fs <*> as

names :: [String]
names = ["Joe", "Sara", "Mae"]

phones :: [String]
phones = ["555-5555", "123-456-7890", "555-4321"]

employees1 :: [Employee]
employees1 = Employee <$> names <*> phones

(.+) :: Applicative a => a Integer -> a Integer -> a Integer
(.+) = liftA2 (+)

(.*) :: Applicative a => a Integer -> a Integer -> a Integer
(.*) = liftA2 (*)

newtype MyZipList a = MyZipList {myGetZipList :: [a]}
  deriving (Eq, Show, Functor)

instance Applicative MyZipList where
  pure = MyZipList . repeat
  MyZipList fs <*> MyZipList xs = MyZipList (zipWith ($) fs xs)

employees2 :: [Employee]
employees2 = myGetZipList $ Employee <$> MyZipList names <*> MyZipList phones

-- instance Functor ((->) e) where
--   fmap = (.)

-- instance Applicative ((->) e) where
--   pure = const
--   (<*>) f x = \e -> (f e) (x e)

data BigRecord = BR
  { getName :: Name,
    getSSN :: String,
    getSalary :: Integer,
    getPhone :: String,
    getLicensePlate :: String,
    getNumSickDays :: Int
  }

r :: BigRecord
r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2

getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone

ex01 :: Employee
ex01 = getEmp r

pair :: Applicative f => f a -> f b -> f (a, b)
pair = liftA2 (,)

-- pair fa fb = pure (,) <*> fa <*> fb
-- pair fa fb = (,) <$> fa <*> fb
-- pair fa fb = (\x y -> (x, y)) <$> fa <*> fb

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 . const $ id

-- (*>) = liftA2 (const id)
-- (*>) fa fb = (const id) <$> fa <*> fb

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f = foldr (liftA2 (:) . f) $ pure []

-- mapA f = (liftA2 (:) . f) `foldr` (pure [])
-- mapA f = (`foldr` (pure [])) (liftA2 (:) . f)
-- mapA f = (`foldr` (pure [])) . (liftA2 (:) .) $ f
-- mapA = (`foldr` (pure [])) . (liftA2 (:) .) -- 이게...하스켈?

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) $ pure []

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = liftA . replicate

-- replicateA n = liftA (replicate n)
-- replicateA n = liftA . replicate $ n
