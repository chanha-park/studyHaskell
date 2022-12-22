{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

-- Exercise 2
type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}
  deriving (Show)

compareRoll :: [DieValue] -> [DieValue] -> (Int -> Int, Int -> Int)
compareRoll [] _ = ((+) 0, (+) 0)
compareRoll _ [] = ((+) 0, (+) 0)
compareRoll (x : xs) (y : ys)
  | x > y = ((+) 0 . f, (+) (-1) . g)
  | otherwise = ((+) (-1) . f, (+) 0 . g)
  where
    (f, g) = compareRoll xs ys

-- How can i change it to >>= form?
battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield x y) = do
  rollAttackers <- throwAttackers x
  rollDefenders <- throwDefenders y
  let (c, d) = compareRoll rollAttackers rollDefenders
  return (Battlefield (c x) (d y))

rsort :: Ord a => [a] -> [a]
rsort = sortBy . flip $ compare

-- throw n = unDV <$> ((fmap maximum) . (replicateM n) $ die)
throw :: Int -> Rand StdGen [DieValue]
throw n = rsort <$> replicateM n die

throwAttackers :: Int -> Rand StdGen [DieValue]
throwAttackers = throw . min 3 . (+ (-1))

throwDefenders :: Int -> Rand StdGen [DieValue]
throwDefenders = throw . min 2

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade x
  | attackers x < 2 || defenders x < 1 = pure x
  | otherwise = battle x >>= invade

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb x =
  simulate x >>= \success -> return (fromIntegral success / fromIntegral numOfInvasion)

isInvasionSuccess :: Battlefield -> Rand StdGen Bool
isInvasionSuccess x =
  invade x >>= \case
    Battlefield _ 0 -> return True
    _ -> return False

isSuccess :: Battlefield -> Bool
isSuccess (Battlefield _ 0) = True
isSuccess _ = False

numOfInvasion :: Int
numOfInvasion = 1000

simulate :: Battlefield -> Rand StdGen Int
simulate x = length . filter (&& True) <$> (replicateM numOfInvasion . isInvasionSuccess $ x)

testBattleField :: Battlefield
testBattleField = Battlefield 5 4

-- b = sequenceA . replicateM 10 invade
-- c = b testBattleField
-- how can i get result from c???

main :: IO ()
main =
  (evalRandIO . successProb $ testBattleField) >>= print

-- same result
-- main = do
--   result <- evalRandIO . successProb $ testBattleField
--   putStrLn . show $ result
--   return ()

-- evalRandIO :: Rand StdGen a -> IO a

-- Exercise 5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
