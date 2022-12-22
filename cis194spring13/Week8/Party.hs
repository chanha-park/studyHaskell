{-# OPTIONS_GHC -Wall -Werror -Wextra -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Tree
import Employee

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
treeFold f (Node a b) = f a (fmap (treeFold f) b)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp xs = (glCons emp $ foldMap snd xs, foldMap fst xs)

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
main :: IO ()
main = readFile "company.txt" >>= putStrLn . showGL . maxFun . read

-- main = readFile "company.txt" >>= \content -> putStrLn . showGL . maxFun . read $ content
-- main = readFile "company.txt" >>= \content -> putStrLn . showGL . maxFun . reify . read $ content

-- reify :: Tree Employee -> Tree Employee
-- reify = id

showGL :: GuestList -> String
showGL (GL xs fun) = unlines $ ("Total fun: " <> (show fun)) : (sort . map empName $ xs)
