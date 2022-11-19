{-# OPTIONS_GHC -Wall -Werror -Wextra #-}
{-# LANGUAGE FlexibleInstances #-}

-- Exercise 6

import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

-- data VarExprT = Lit Integer
--               | Var String
--               | Add VarExprT VarExprT
--               | Mul VarExprT VarExprT
--   deriving (Show, Eq)

-- instance Expr VarExprT where
--   lit x = Lit x
--   add x y = Add x y
--   mul x y = Mul x y

-- instance HasVars VarExprT where
--   var x = Var x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- study applicative functors
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \_ -> Just x
  add f g = \m -> (+) <$> (f m) <*> (g m)
  mul f g = \m -> (*) <$> (f m) <*> (g m)

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs ex = ex $ M.fromList vs
