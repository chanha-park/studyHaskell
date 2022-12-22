{-# OPTIONS_GHC -Wall -Werror -Wextra #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scrabble where

import Data.Char

-- Exercise 3
newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

class Scored a where
  scored :: a -> Score

instance Scored Score where
  scored = id

instance Scored a => Scored (a, b) where
  scored = scored . fst

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score x
  | (elem . toLower) x "aeilnorstu" = Score 1
  | (elem . toLower) x "dg" = Score 2
  | (elem . toLower) x "bcmp" = Score 3
  | (elem . toLower) x "hvwy" = Score 4
  | (elem . toLower) x "k" = Score 5
  | (elem . toLower) x "jx" = Score 8
  | (elem . toLower) x "qz" = Score 10
  | otherwise = Score 0

scoreString :: String -> Score
scoreString = mconcat . fmap score
