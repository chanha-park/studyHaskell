-- x:: Int
-- x = 3

-- y :: Int -> Int
-- y x = x + 1

-- biggestInt, smallestInt :: Int
-- biggestInt = maxBound
-- smallestInt = minBound

-- n::Integer
-- n = 12312914927349274293742934723948729472389743298743297

-- m::Integer
-- m = 2^(2^(2^(2^2)))

-- numDigits :: Int
-- numDigits = length (show m)

-- d1, d2 :: Double
-- d1 = 4.234235
-- d2 = 6.234e-5

-- sumtorial :: Integer -> Integer
-- sumtorial 0 = 0
-- sumtorial n = n + sumtorial(n - 1)

-- hailstone :: Integer -> Integer
-- hailstone n
--     | (mod n 2 == 0) = (div n 2)
--     | otherwise = (3 * n +1 )

-- hailstoneSeq :: Integer -> [Integer]
-- hailstoneSeq 1 = 1 : []
-- hailstoneSeq n = n : hailstoneSeq(hailstone n)

-- foo :: Integer -> Integer
-- foo 0 = 16
-- foo 1
--     | "Hello" > "C++" = 3
--     | otherwise = 4
-- foo n
--     | n < 0 = 0
--     | (mod n 17 == 2) = -43
--     | otherwise = n + 3

-- p :: (Integer, Char)
-- p = (3, 'x')

-- f :: Integer -> Integer -> Integer -> Integer
-- f x y z = x + y + z

-- intListLength :: [Integer] -> Integer
-- intListLength [] = 0
-- intListLength (x : xs) = 1 + intListLength(xs)

-- sumEveryTwo :: [Integer] -> [Integer]
-- sumEveryTwo [] = []
-- sumEveryTwo (x : []) = [x]
-- sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

-- hailstoneLen :: Integer -> Integer
-- hailstoneLen n = intListLength (hailstoneSeq n) - 1

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | x < 10 = [x]
    | otherwise = (toDigits (div x 10)) ++ [mod x 10]

reverseDigits :: [Integer] -> [Integer]
reverseDigits [] = []
reverseDigits (x : xs) = (reverseDigits xs) ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverseDigits (toDigits x)

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft [] = []
doubleFromLeft (x : []) = x : []
doubleFromLeft (x:(y:zs)) = (x : (2 * y) : []) ++ doubleFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverseDigits (doubleFromLeft (reverseDigits x ))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = (div x 10) + (mod x 10) + sumDigits(xs)

validate :: Integer -> Bool
validate x = (mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0)

-- move n disks from Peg a to Peg b. Peg c is tmp storage

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = (a, b) : []
hanoi n a b c = (hanoi (n - 1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n - 1) c b a)
