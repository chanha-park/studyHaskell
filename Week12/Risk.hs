{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

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

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield x y) = pure (Battlefield x' y')
  where
    (x', y') = fight (x, y)

fight :: (Army, Army) -> (Army, Army)
fight = undefined

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade x
  | attackers x < 2 = pure x
  | otherwise = battle x >>= invade

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb = undefined

numOfInvasion :: Int
numOfInvasion = 1000

-- Exercise 5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined

-- evalRandIO :: Rand StdGen a -> IO a
