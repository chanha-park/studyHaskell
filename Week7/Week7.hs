{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty        = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r


treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l)  x (treeFold e f r)


treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> l + x + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l x r -> l `max` x `max` r)

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

numLiterals :: ExprT -> Int
numLiterals = exprTFold (\_ -> 1) (+) (+)
-- numLiterals = exprTFold (const 1) (+) (+)

class MyMonoid m where
  myMempty :: m
  myMappend :: m -> m -> m
  myMconcat :: [m] -> m
  myMconcat = foldr myMappend myMempty
  (<>) :: MyMonoid m => m -> m -> m
  (<>) = myMappend

instance MyMonoid [a] where
  myMempty = []
  myMappend = (++)

newtype Sum a = Sum a
    deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => MyMonoid (Sum a) where
  myMempty = Sum 0
  myMappend = (+)

newtype Product a = Product a
    deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => MyMonoid (Product a) where
  myMempty = Product 1
  myMappend = (*)

lst :: [Integer]
lst = [1, 5, 8, 223, 423, 99]

prod :: Integer
prod = getProduct . myMconcat . map Product $ lst

sum :: Integer
sum = getSum . myMconcat . map Sum $ lst

instance (MyMonoid a, MyMonoid b) => MyMonoid (a, b) where
  myMempty = (myMempty, myMempty)
  (a, b) `myMappend` (c, d) = (a `myMappend` c, b `myMappend` d)

newtype Or a = Or a
    deriving (Eq, Ord, Num, Show)

getOr :: Or a -> a
getOr (Or a) = a

instance MyMonoid (Or Bool) where
  myMempty = Or False
  myMappend (Or x) (Or y) = Or (x || y)

newtype And a = And a
    deriving (Eq, Ord, Num, Show)

getAnd :: And a -> a
getAnd (And a) = a

instance MyMonoid (And Bool) where
  myMempty = And True
  myMappend (And x) (And y) = And (x && y)

myAnd :: [Bool] -> Bool
myAnd = getAnd . myMconcat . map And

myOr :: [Bool] -> Bool
myOr = getOr . myMconcat . map Or

-- monoid for function type
instance MyMonoid (a -> a) where
  myMempty = id
  myMappend x y = x . y

