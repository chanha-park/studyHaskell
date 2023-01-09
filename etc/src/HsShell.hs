{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module HsShell (
    main,
    myLoop,
) where

import System.Exit
import System.IO

import HsShell.Execute (execute)
import HsShell.Expand (expand)
import HsShell.Parse (parse)

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
                    else getLine >>= (execute . expand . parse) >> myLoop
            )

printPrompt :: IO ()
printPrompt = putStr "Input $ "
