{-# OPTIONS_GHC -Wall -Werror -Wextra -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree
import Data.List

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps f) = GL (emp : emps) (f + empFun emp)

instance Semigroup GuestList where
  (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend = (<>)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

-- data Tree a = Node {
--         rootLabel :: a,
--         subForest :: [Tree a]
--    }

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f x@(Node {subForest = []}) = f (rootLabel x) []
treeFold f x = f (rootLabel x) (map (treeFold f) (subForest x))

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp [] = (GL [emp] (empFun emp), GL [] 0)
nextLevel emp xs = (maxWith, maxWithOut)
  where
    maxWith = glCons emp (maximum [snd x | x <- xs])
    maxWithOut = maximum [fst x | x <- xs]

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun x = max (fst tmp) (snd tmp)
  where tmp = treeFold nextLevel x

-- Exercise 5
main :: IO ()
main = readFile "company.txt" >>= \content -> putStrLn . showGL . maxFun . read $ content

-- main = readFile "company.txt" >>= \content -> putStrLn . showGL . maxFun . reify . read $ content
-- reify :: Tree Employee -> Tree Employee
-- reify = id

showGL :: GuestList -> String
showGL (GL xs fun) = unlines $ ("Total fun: " <> (show fun)) : (sort . map empName $ xs)
