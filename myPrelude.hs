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
