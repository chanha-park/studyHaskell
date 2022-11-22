{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

class Animal a where
  makeSound :: a -> String

data Dog = Dog String
    deriving (Show)

data Cat = Cat String
    deriving (Show)

instance Animal Dog where
  makeSound (Dog name) = "This is Dog " ++ name

instance Animal Cat where
  makeSound (Cat name) = "This is Cat" ++ name

main :: IO()
main = do
    let a = Dog "hotdog"
    putStrLn (makeSound a)
