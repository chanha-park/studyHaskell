{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

import System.Exit
import System.IO

-- import System.Process

main :: IO ()
main = myLoop

myLoop :: IO ()
myLoop =
    printPrompt
        >> isEOF
        >>= ( \eof ->
                if eof
                    then exitWith (ExitFailure 1)
                    else getLine >>= (execute . parse) >> myLoop
            )

parse :: String -> String
parse = id

execute :: String -> IO ()
execute = putStrLn . (++ "$")

printPrompt :: IO ()
printPrompt = putStr "Input $ "
