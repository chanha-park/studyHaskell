{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module HsBlog.Convert (
    process,
    convert,
    convertStructure,
) where

import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
        Markup.Heading n txt -> Html.h_ n $ Html.txt_ txt
        Markup.Paragraph p -> Html.p_ $ Html.txt_ p
        Markup.UnorderedList list -> Html.ul_ $ map (Html.p_ . Html.txt_) list
        Markup.OrderedList list -> Html.ol_ $ map (Html.p_ . Html.txt_) list
        Markup.CodeBlock list -> Html.code_ (unlines list)

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

-- process title content = Html.render $ convert title (Markup.parse content)

-- func :: Markup.Document -> Html.Structure
-- func = foldMap convertStructure
-- func = foldr ((<>) . convertStructure) mempty

-- concatStructure :: [Html.Structure] -> Html.Structure
-- concatStructure list =
--     case list of
--         [] -> mempty
--         x : xs -> x <> concatStructure xs
