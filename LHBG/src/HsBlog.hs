{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

module HsBlog (
    main,
    process,
)
where

import HsBlog.Convert (process)

-- import qualified HsBlog.Html as Html
-- import qualified HsBlog.Markup as Markup

import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> noArgs
        [x, y] -> twoArgs x y
        _ -> explainUsage

-- main =
--     getArgs
--         >>= ( \case
--                 [] -> noArgs
--                 [x, y] -> twoArgs x y
--                 _ -> explainUsage
--             )

noArgs :: IO ()
noArgs = do
    content <- getContents
    putStrLn $ process "Untitled" content

-- noArgs = getContents >>= (putStrLn . process "Untitled")

explainUsage :: IO ()
explainUsage =
    putStrLn
        ( unlines
            [ "No args: stdin -> stdout"
            , "Two Args: InputFile OutputFile"
            ]
        )

confirm :: String -> IO Bool
confirm msg =
    putStrLn msg
        *> getLine
        >>= \case
            "y" -> pure True
            "n" -> pure False
            _ ->
                putStrLn "Invalid response. use y or n"
                    *> confirm msg

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = cond >>= \result -> when result action

twoArgs :: String -> String -> IO ()
twoArgs infile outfile = do
    check <- doesFileExist outfile
    if check
        then askAndConvert infile outfile
        else convertFile infile outfile

-- twoArgs infile outfile =
--     doesFileExist outfile
--         >>= ( \check ->
--                 if check
--                     then askAndConvert infile outfile
--                     else convertFile infile outfile
--             )

askAndConvert :: String -> String -> IO ()
askAndConvert infile = whenIO (confirm "File already Exists. Want to Overwrite? (y/n)") . convertFile infile

convertFile :: String -> String -> IO ()
convertFile infile outfile = do
    content <- readFile infile
    writeFile outfile (process infile content)
    putStrLn "Done"

-- convertFile infile outfile = readFile infile >>= (writeFile outfile . process infile) >> putStrLn "Done"
