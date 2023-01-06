{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module Convert (
    process,
) where

import Html qualified
import Markup qualified

convert :: Html.Title -> Markup.Document -> Html.Html
convert x = Html.html_ x . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
        Markup.Heading n txt -> Html.h_ n txt
        Markup.Paragraph p -> Html.p_ p
        Markup.UnorderedList list -> Html.ul_ $ map Html.p_ list
        Markup.OrderedList list -> Html.ol_ $ map Html.p_ list
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
