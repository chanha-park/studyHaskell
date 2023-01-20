{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module HsBlog.Convert (
    process,
    buildIndex,
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

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
    Html.html_
        "Blog"
        ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
            <> Html.h_ 2 (Html.txt_ "Posts")
            <> mconcat previews
        )
  where
    previews =
        map
            ( \(file, doc) -> case doc of
                Markup.Heading 1 heading : article ->
                    Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                        <> foldMap convertStructure (take 3 article)
                        <> Html.p_ (Html.link_ file (Html.txt_ "..."))
                _ -> Html.h_ 3 (Html.link_ file (Html.txt_ file))
            )
            files

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
