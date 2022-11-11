{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

data Thing = Shoe
    | Ship
    | SealingWax
    | Cabbage
    | King
    deriving Show

shoe :: Thing
shoe = Shoe

listOfThings :: [Thing]
listOfThings = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _ = True

data FailableDouble = Failure
                    | OK Double
                deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d -> d

data Person = Person String Integer Thing
    deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Integer
getAge (Person _ a _ ) = a

data AlgDataType = Constr1 String Integer
                 | Constr2 Integer
                 | Constr3 [Integer]
                 | Constr4

foo :: AlgDataType -> Integer
foo (Constr1 _ b) = 4 + b
foo (Constr2 _) = 2
foo (Constr3 _) = 3
foo (Constr4) = 4

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _) = n ++ ", your favorite thing is lame."

data IntList = Empty
             | Cons Integer IntList

makeIntList :: [Integer] -> IntList
makeIntList [] = Empty
makeIntList (x : xs) = Cons x (makeIntList xs)

intListProd :: IntList -> Integer
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

data Tree = Leaf Char
          | Node Tree Int Tree
          deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
