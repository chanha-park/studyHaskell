{-# OPTIONS_GHC -Wall -Wextra #-}

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insertPos x (insertionSort xs)
  where
    insertPos y [] = [y]
    insertPos y (z : zs) = if y <= z then y : z : zs
                                     else z : insertPos y zs

ex1 :: [Int]
ex1 = [3, 9, -4, 0, 8]

ex2 :: [String]
ex2 = ["hello", "world", "bar", "foo", "43", "12"]

data Student = Student
  { index :: Int,
    name :: String
  }
  deriving (Eq, Ord, Show)

ex3 :: [Student]
ex3 = [Student 9 "alex", Student {index = 3, name = "Zar"}]

main :: IO ()
main = do
  print . insertionSort $ ex1
  print . insertionSort $ ex2
  print . insertionSort $ ex3
