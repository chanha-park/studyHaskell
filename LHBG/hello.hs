{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

import Html

main :: IO ()
main = putStrLn . render $ myhtml

myhtml :: Html
myhtml =
    html_
        "my title"
        ( h1_ "my heading"
            <> p_ "paragraph 1"
            <> p_ "paragraph 2"
        )
