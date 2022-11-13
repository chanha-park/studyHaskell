-- custom foldr, foldl implementation
-- foldr', foldl' ???

myFoldr :: (a -> b -> b ) -> b -> [a] -> b
myFoldr fn acc [] = acc
myFoldr fn acc (x : xs) = fn x (myFoldr fn acc xs)

myFoldl :: (b -> a -> b ) -> b -> [a] -> b
myFoldl fn acc [] = acc
myFoldl fn acc (x : xs) = myFoldl fn (fn acc x) xs
