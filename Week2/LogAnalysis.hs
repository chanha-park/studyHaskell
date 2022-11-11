{-# OPTIONS_GHC -Wall -Werror -Wextra #-}

module LogAnalysis where

import Log
import Data.Char (ord)

trimString :: String -> String
trimString (' ' : xs) = trimString xs
trimString x = x

isDigit :: Char -> Bool
isDigit c
  | (c >= '0') && (c <= '9') = True
  | otherwise = False

stringToInt :: String -> Int -> Pair
stringToInt [] n = Pair n []
stringToInt (x : xs) n
  | isDigit x = stringToInt xs (10 * n + ord(x) - 48)
  | otherwise = Pair n (trimString (x : xs))

atoi :: String -> Pair
atoi x = stringToInt (trimString x) 0

data Pair = Pair Int String

getFirst :: Pair -> Int
getFirst (Pair x _) = x

getSecond :: Pair -> String
getSecond (Pair _ y) = y

parseMessage :: String -> LogMessage
parseMessage ('E' : ' ': xs) = LogMessage (Error level) time msg
  where 
    level = getFirst (atoi xs)
    stringRemain = getSecond (atoi xs)
    time = getFirst (atoi stringRemain)
    msg = getSecond (atoi stringRemain)
parseMessage ('I' : ' ' : xs) = LogMessage Info time msg
  where 
    time = getFirst (atoi xs)
    msg = getSecond (atoi xs)
parseMessage ('W' : ' ' : xs) = LogMessage Warning time msg
  where 
    time = getFirst (atoi xs)
    msg = getSecond (atoi xs)
parseMessage x = Unknown x

parse :: String -> [LogMessage]
parse x = map (parseMessage) (lines x)

getTimeStamp :: LogMessage -> Int
getTimeStamp (LogMessage _ time _) = time
getTimeStamp _ = 0

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ x) = x
getMessage (Unknown x) = x

getSeverity :: LogMessage -> Int
getSeverity (LogMessage (Error level) _ _) = level
getSeverity _ = 0

getLeft :: MessageTree -> MessageTree
getLeft (Node left _ _) = left
getLeft _ = Leaf

getRight :: MessageTree -> MessageTree
getRight (Node _ _ right) = right
getRight _ = Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tr = tr
insert msg (Node l content r)
  | (getTimeStamp msg) < (getTimeStamp msg) = (Node (insert msg l) content r)
  | otherwise = (Node l content (insert msg r))
insert _ x = x

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x : xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node b c d) = (inOrder b) ++ [c] ++ (inOrder d)

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error x) _ _) = (x >= 50)
isRelevant _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (x : xs)
  | isRelevant x = (getMessage x) : whatWentWrong xs
  | otherwise = whatWentWrong xs
