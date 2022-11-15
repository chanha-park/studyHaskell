-- custom Prelude Function implementation
-- foldr', foldl' ???

myFoldr :: (a -> b -> b ) -> b -> [a] -> b
myFoldr fn acc [] = acc
myFoldr fn acc (x : xs) = fn x (myFoldr fn acc xs)

myFoldl :: (b -> a -> b ) -> b -> [a] -> b
myFoldl fn acc [] = acc
myFoldl fn acc (x : xs) = myFoldl fn (fn acc x) xs

myMap :: (a -> b) -> [a] -> [b]
myMap fn [] = []
myMap fn (x : xs) = (fn x) : (myMap fn xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fn [] = []
myFilter fn (x : xs)
  | fn x = x : (myFilter fn xs)
  | otherwise = myFilter fn xs

myLength :: [a] -> Int
myLength = myFoldr (\x y -> 1 + y) 0

mySum :: Num a => [a] -> a
mySum = myFoldr (+) 0

myProduct :: Num a => [a] -> a
myProduct = myFoldr (*) 1

myAnd :: [Bool] -> Bool
myAnd = myFoldr (&&) True

myOr :: [Bool] -> Bool
myOr = myFoldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny fn = myOr . map fn

myAll :: (a -> Bool) -> [a] -> Bool
myAll fn = myAnd . map fn
