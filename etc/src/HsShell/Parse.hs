{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

module HsShell.Parse where

import HsShell.Syntax

parse :: String -> Command
parse = Command
