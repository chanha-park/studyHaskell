{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

-- module MyHtml where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

main :: IO ()
main = putStrLn . render $ myhtml

myhtml :: Html
myhtml = html_ "my title" (h1_ "my heading" `append_` p_ "paragraph 1" `append_` p_ "paragraph 2")

-- makeHtml :: String -> String -> String
-- makeHtml x y = html_ (head_ (title_ x) `append_` body_ y)

-- makeHtml = (.) html_ . (. body_) . (<>) . head_ . title_ -- PointFree?lkjfojefe

html_ :: Title -> Structure -> Html
html_ x y = Html (el "html" ( el "head" (el "title" x) <> el "body" (getStructureString y)))

-- body_ :: Body -> Structure
-- body_ = Structure . el "body"

-- head_ :: Head -> Structure
-- head_ = Structure . el "head"

-- title_ :: Title -> Structure
-- title_ = Structure . el "title"

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el x y = "<" <> x <> ">" <> y <> "</" <> x <> ">"

append_ :: Structure -> Structure -> Structure
append_ (Structure x) (Structure y) = Structure (x <> y)

getStructureString :: Structure -> String
getStructureString (Structure x) = x

render :: Html -> String
render (Html x) = x
