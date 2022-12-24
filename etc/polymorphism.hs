{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

class Animal a where
  makeSound :: a -> String
  getName :: a -> String

newtype Dog = Dog String
  deriving (Show)

newtype Cat = Cat String
  deriving (Show)

instance Animal Dog where
  getName (Dog name) = name
  makeSound = ("This is Dog " ++) . getName

instance Animal Cat where
  getName (Cat name) = name
  makeSound = ("This is Cat " ++) . getName

main :: IO ()
main = (putStrLn . makeSound $ a) >> (putStrLn . makeSound $ b)
  where
    a = Dog "hotdog"
    b = Cat "concat"

-- main = do
--   let a = Dog "hotdog"
--   let b = Cat "concat"
--   putStrLn (makeSound a)
--   putStrLn (makeSound b)
