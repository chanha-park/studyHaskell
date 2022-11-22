{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

-- main :: IO ()

-- main 1
-- main = putStrLn "Hello, Haskell!" >> putStrLn "wow"

-- main 2
-- main = putStrLn "Enter a Number" >> (readLn >>= (\n -> putStrLn (show (n + 1))))

-- main 3
-- data D = C T1 T2 T3

-- Practice record syntax
data D = C { field1 :: String, field2 :: Int, field3 :: [String] }
    deriving (Show)

a :: D
a = C "a" 3 ["b", "c"]

b :: Int
b = field2 a

c :: D
c = C {field3 = ["hello", "world"], field2 = length (field3 c), field1 = head (field3 c) }

d :: D
d = c {field1 = "newName"}

foo :: D -> String
foo (C {field1 = "hello"}) = "field1 of x is hello"
foo _ = "field1 of x is not hello"
