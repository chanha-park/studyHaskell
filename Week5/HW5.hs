{-# OPTIONS_GHC -Wall -Werror -Wextra #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import Parser (parseExp)
import StackVM
import Data.Maybe
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit a) = a
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = (\x -> case x of
                  Just n -> Just (eval n)
                  _ -> Nothing) . (parseExp ExprT.Lit ExprT.Add ExprT.Mul)

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = ExprT.Lit x
  add x y = ExprT.Add x y
  mul x y = ExprT.Mul x y

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




-- Exercise5

instance Expr Program where
  lit x = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile x = parseExp lit add mul x

testStackVM :: String -> Either String StackVal
testStackVM = stackVM . fromMaybe [] . compile


-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | Var String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit x = VLit x
  add x y = VAdd x y
  mul x y = VMul x y

instance HasVars VarExprT where
  var x = Var x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- study applicative functors
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \_ -> Just x
  add f g = \m -> (+) <$> (f m) <*> (g m)
  mul f g = \m -> (*) <$> (f m) <*> (g m)

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs expr = expr $ M.fromList vs



