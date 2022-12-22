{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
first :: (a -> b) -> (a, c) -> (b, c)
first f = (,) <$> (f . fst) <*> snd

-- first f x = (f . fst $ x, snd x)

second :: (a -> b) -> (c, a) -> (c, b)
second f = (,) <$> fst <*> (f . snd)

-- second f x = (fst x, f . snd $ x)

instance Functor Parser where
  fmap f x = Parser $ fmap (first f) . runParser x

-- Belows are equivalent
-- fmap f x = Parser (fmap (first f) . runParser x)
-- fmap f x = Parser (fmap (first f) . (runParser x))
-- fmap f (Parser fn) = Parser (fmap (first f) . fn)
-- fmap f (Parser fn) = Parser (\x -> fmap (first f) . fn $ x)
-- fmap f (Parser fn) = Parser (\x -> (fmap (first f)) (fn x))
-- fmap f (Parser fn) = Parser (\x -> fmap (first f) (fn x))

-- Exercise 2
instance Applicative Parser where
  pure x = Parser $ \y -> Just (x, y)
  (<*>) p1 p2 = Parser fn'
    where
      fn' = \str -> runParser p1 str >>= fn''
      fn'' = \(p, str') -> runParser (fmap p p2) str'

-- (<*>) (Parser fn1) p2 = Parser fn'
--   where
--     fn' = \str -> case fn1 str of
--       Nothing -> Nothing
--       Just (p', str') -> runParser (fmap p' p2) $ str'

-- (<*>) (Parser fn1) (Parser fn2) = Parser fn'
--   where
--     fn' = \str -> case fn1 str of
--       Nothing -> Nothing
--       Just (p', str') -> fmap (first p') . fn2 $ str'

-- (<*>) (Parser fn1) (Parser fn2) = Parser fn'
--   where
--     fn' = \str -> case (fn1 str) of
--       Nothing -> Nothing
--       Just (p', str') -> case (fn2 str') of
--         Nothing -> Nothing
--         Just (q, r) -> Just (p' q, r)

type Name = String

data Employee = Emp {name :: Name, phone :: String}
  deriving (Show)

parseName :: Parser Name
parseName = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isAlpha xs

parsePhone :: Parser String
parsePhone = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

parseEmployee :: Parser Employee
parseEmployee = Emp <$> parseName <*> parsePhone

-- Exercise 3
abParser :: Parser (Char, Char)
abParser = (fmap (,) (char 'a')) <*> (char 'b')

-- abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = fmap (const ()) abParser

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4
-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  empty = Parser . const $ Nothing
  (<|>) (Parser p1) (Parser p2) = Parser $ liftA2 (<|>) p1 p2

-- empty = Parser $ \_ -> Nothing
-- (<|>) p1 p2 = Parser $ \x -> runParser p1 x <|> runParser p2 x

-- Exercise 5
intOrUppercase :: Parser ()
intOrUppercase = (fmap (const ()) posInt) <|> (fmap (const ()) (satisfy isUpper))
