{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

-- Html.Internal.hs

module HsBlog.Html.Internal where

import Numeric.Natural

-- Types

newtype Html = Html String

newtype Structure = Structure String

newtype Content = Content String

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

-- Structure {{{

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ('h' : show n) . getContentString

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

-- }}}

-- Content {{{

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
    Content $
        elAttr
            "a"
            ("href=\"" <> escape path <> "\"")
            (getContentString content)

img_ :: FilePath -> Content
img_ path =
    Content $
        "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ = Content . el "b" . getContentString

i_ :: Content -> Content
i_ = Content . el "i" . getContentString

instance Semigroup Content where
    (<>) x y = Content (getContentString x <> getContentString y)

instance Monoid Content where
    mempty = Content ""

-- }}}

-- render

render :: Html -> String
render (Html x) = x

-- Utils

el :: String -> String -> String
el x y = "<" <> x <> ">" <> y <> "</" <> x <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attr content =
    "<" <> tag <> " " <> attr <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure x) = x

getContentString :: Content -> String
getContentString (Content x) = x

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
