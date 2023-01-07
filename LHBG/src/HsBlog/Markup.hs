{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

module HsBlog.Markup (
    Document,
    Structure (..),
    parse,
) where

import Data.Maybe
import Numeric.Natural

type Document = [Structure]

data Structure
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving (Eq, Show)

data Context
    = CtxHeading Natural String
    | CtxParagraph [String]
    | CtxUnorderedList [String]
    | CtxOrderedList [String]
    | CtxCodeBlock [String]
    deriving (Show)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
    case txts of
        -- terminate
        [] -> maybeToList context
        -- Heading 1
        ('*' : ' ' : line) : rest ->
            maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
        -- UnorderedList
        ('-' : ' ' : line) : rest ->
            case context of
                Just (UnorderedList list) ->
                    parseLines (Just (UnorderedList (list <> [trim line]))) rest
                _ -> maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
        -- OrderedList
        ('#' : ' ' : line) : rest ->
            case context of
                Just (OrderedList list) ->
                    parseLines (Just (OrderedList (list <> [trim line]))) rest
                _ -> maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)
        -- CodeBlock
        ('>' : ' ' : line) : rest ->
            case context of
                Just (CodeBlock code) ->
                    parseLines (Just (CodeBlock (code <> [line]))) rest
                _ -> maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)
        -- Paragraph
        currentLine : rest ->
            let line = trim currentLine
             in if line == ""
                    then maybe id (:) context (parseLines Nothing rest)
                    else case context of
                        Just (Paragraph paragraph) ->
                            parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
                        _ ->
                            maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words

-- example1 :: Document
-- example1 =
--     [ Paragraph "Hello, world!"
--     ]

-- example2 :: Document
-- example2 =
--     [ Heading 1 "Welcome"
--     , Paragraph "To this tutorial about Haskell."
--     ]

-- example3 :: Document
-- example3 =
--     [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
--     , OrderedList
--         [ "Item 1 of a list"
--         , "Item 2 of the same list"
--         ]
--     ]

-- example4 :: Document
-- example4 =
--     [ Heading 1 "Compiling programs with ghc"
--     , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
--     , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
--     , CodeBlock
--         [ "main = putStrLn \"Hello, Haskell!\""
--         ]
--     , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
--     , CodeBlock
--         [ "➜ ghc hello.hs"
--         , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
--         , "Linking hello ..."
--         ]
--     , Paragraph "GHC created the following files:"
--     , UnorderedList
--         [ "hello.hi - Haskell interface file"
--         , "hello.o - Object file, the output of the compiler before linking"
--         , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
--         ]
--     , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
--     , OrderedList
--         [ "Defines the main function in the source file"
--         , "Defines the module name to be Main, or does not have a module declaration"
--         ]
--     , Paragraph "Otherwise, it will only produce the .o and .hi files."
--     ]
