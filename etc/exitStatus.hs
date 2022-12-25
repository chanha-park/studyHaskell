{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

import System.Exit

-- Example with exit status

cutOff :: Int
cutOff = 10

printMsgAndExit :: Int -> IO ()
printMsgAndExit x = putStr msg >> print cutOff >> exitFunc
  where
    (msg, exitFunc)
      | x > cutOff = ("Longer than ", exitSuccess)
      | otherwise = ("Shorter than ", exitWith . ExitFailure $ x)

main :: IO ()
main = getLine >>= return . length >>= printMsgAndExit

-- main =
--   getLine >>= \inputStr ->
--     let k = length inputStr
--      in if k > cutOff
--           then putStrLn ("input longer than " ++ show cutOff) >> exitSuccess
--           else putStrLn ("input shorter than " ++ show cutOff) >> exitWith (ExitFailure k)

-- main = do
--   inputStr <- getLine
--   let k = length inputStr
--   if k > 5
--     then exitSuccess
--     else exitWith (ExitFailure 3)
