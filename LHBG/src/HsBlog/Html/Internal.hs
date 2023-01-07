{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

-- Html.Internal.hs

module HsBlog.Html.Internal where

import Numeric.Natural

-- Types

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- EDSL

html_ :: Title -> Structure -> Html
html_ x y =
    Html
        ( el
            "html"
            ( el
                "head"
                (el "title" . escape $ x)
                <> el "body" (getStructureString y)
            )
        )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

-- h1_ :: String -> Structure
-- h1_ = Structure . el "h1" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ('h' : show n) . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

instance Semigroup Structure where
    (<>) x y = Structure (getStructureString x <> getStructureString y)

instance Monoid Structure where
    mempty = Structure ""

-- append_ :: Structure -> Structure -> Structure
-- append_ (Structure x) (Structure y) = Structure (x <> y)

-- empty_ :: Structure
-- empty_ = Structure ""

-- render

render :: Html -> String
render (Html x) = x

-- Utils

el :: String -> String -> String
el x y = "<" <> x <> ">" <> y <> "</" <> x <> ">"

getStructureString :: Structure -> String
getStructureString (Structure x) = x

escape :: String -> String
escape =
    let
        escapeChar c = case c of
            '<' -> "&lt;"
            '>' -> "&gt;"
            '&' -> "&amp;"
            '"' -> "&quot;"
            '\'' -> "&#39;"
            _ -> [c]
     in
        concatMap escapeChar
