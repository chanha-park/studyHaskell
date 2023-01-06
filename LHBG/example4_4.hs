{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

import Data.Maybe

data Brightness
    = Dark
    | Bright

data EightColor
    = Black
    | Red

-- \| Green
-- \| Yellow
-- \| Blue
-- \| Magenta
-- \| Cyan
-- \| White

data AnsiColor
    = AnsiColor Brightness EightColor

-- | A data type representing colors
data Color
    = RGB Int Int Int

getBluePart :: Color -> Int
getBluePart color =
    case color of
        RGB _ _ blue -> blue

ansiColorToVGA :: AnsiColor -> Color
ansiColorToVGA ansicolor =
    case ansicolor of
        AnsiColor Dark Black -> RGB 0 0 0
        AnsiColor Bright Black -> RGB 85 85 85
        AnsiColor Dark Red -> RGB 170 0 0
        AnsiColor Bright Red -> RGB 255 85 85

isBright :: AnsiColor -> Bool
isBright ansicolor =
    case ansicolor of
        AnsiColor Bright _ -> True
        _ -> False

ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansicolor =
    case ansicolor of
        AnsiColor Dark Black -> RGB 1 1 1
        AnsiColor Bright Black -> RGB 128 128 128
        AnsiColor Dark Red -> RGB 222 56 43
        AnsiColor Bright Red -> RGB 255 0 0

isEmpty :: [a] -> Bool
isEmpty xs =
    case listToMaybe xs of
        Just _ -> False
        Nothing -> True

isEmpty' :: [a] -> Bool
isEmpty' [] = True
isEmpty' _ = False
