{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

module Week10 where

type Name = String

data Employee = Employee { name :: Name, phone :: String }
    deriving Show

-- Employee :: Name -> String -> Employee

-- fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
-- fmap2 h fa fb = undefined

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Week10.Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  Just f <*> Just x = Just (f x)

m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

-- ex01 = Employee <$> m_name1 <*> m_phone1
-- ex02 = Employee <$> m_name1 <*> m_phone2
-- ex03 = Employee <$> m_name2 <*> m_phone1
-- ex04 = Employee <$> m_name2 <*> m_phone2
