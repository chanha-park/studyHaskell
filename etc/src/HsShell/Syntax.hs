{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module HsShell.Syntax where

newtype Command = Command String

getString :: Command -> String
getString (Command str) = str
