{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

module HsShell.Expand where

import HsShell.Syntax

expand :: Command -> Command
expand = id
