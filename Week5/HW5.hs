{-# OPTIONS_GHC -Wall -Werror -Wextra #-}
{-# LANGUAGE TypeSynonymInstances #-}

import ExprT
import Parser (parseExp)

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = (\x -> case x of
                  Just n -> Just (eval n)
                  _ -> Nothing) . (parseExp Lit Add Mul)

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = lit (max x y)
  mul (MinMax x) (MinMax y) = lit (min x y)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)

newtype MinMax = MinMax Integer
    deriving (Eq, Show)

newtype Mod7 = Mod7 Integer
    deriving (Eq, Show)

-- testExp :: Expr a => Maybe a
-- testExp = parseExp lit add mul "(3 * -4) + 5"

-- testInteger = testExp :: Maybe Integer
-- testBool = testExp :: Maybe Bool
-- testMM = testExp :: Maybe MinMax
-- testSat = testExp :: Maybe Mod7
