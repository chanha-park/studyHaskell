{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

module HsShell.Execute where

import HsShell.Syntax

execute :: Command -> IO ()
execute = putStrLn . (++ "$") . getString
