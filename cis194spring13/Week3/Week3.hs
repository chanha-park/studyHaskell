{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

data IntList = Empty | Cons Int IntList
    deriving Show

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList fn (Cons x xs) = Cons (fn x) (mapIntList fn xs)

a :: IntList
a = Cons 3 (Cons (-2) (Cons 5 Empty))

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
  | p x = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (t -> s) -> (List t) -> (List s)
mapList _ E = E
mapList fn (C x xs) = C (fn x) (mapList fn xs)

doStuff1 :: [Int] -> Int
doStuff1 [] = 0
doStuff1 [_] = 0
doStuff1 xs = head xs + head (tail xs)

doStuff2 :: [Int] -> Int
doStuff2 [] = 0
doStuff2 [_] = 0
doStuff2 (x1 : x2 : _) = x1 + x2

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x


data NonEmptyList a = NEL a [a]
nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x : xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel [] = Nothing
listToNel (x : xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL x _) = x

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
