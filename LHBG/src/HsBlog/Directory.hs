{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module HsBlog.Directory (
    convertDirectory,
    buildIndex,
) where

import HsBlog.Convert (convert, convertStructure)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

import Control.Monad (void, when)
import Data.List (partition)
import Data.Traversable (for)

import Control.Exception (SomeException (..), catch, displayException)
import System.Directory (
    copyFile,
    createDirectory,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
 )
import System.Exit (exitFailure)
import System.FilePath (
    takeBaseName,
    takeExtension,
    takeFileName,
    (<.>),
    (</>),
 )
import System.IO (hPutStrLn, stderr)

-- WIP --

checkTxtExtension :: [FilePath] -> ([FilePath], [FilePath])
checkTxtExtension = partition ((== ".txt") . takeExtension)

constructDirContents :: ([FilePath], [FilePath]) -> DirContents
constructDirContents (xs, ys) = DirContents (splitBaseExtension xs) ys
  where
    splitBaseExtension :: [FilePath] -> [(FilePath, String)]
    splitBaseExtension = map (\x -> (takeBaseName x, takeExtension x))

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir =
    listDirectory inputDir
        <&> map (inputDir </>)
        <&> checkTxtExtension
        <&> constructDirContents

-- WIP --

data DirContents = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
    , dcFilesToCopy :: [FilePath]
    }

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

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
    DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
    createOutputDirectoryOrExit outputDir
    let
        outputHtmls = txtsToRenderedHtml filesToProcess
    copyFiles outputDir filesToCopy
    writeFiles outputDir outputHtmls
    putStrLn "Done."

createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit = undefined

txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml = undefined

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles = undefined

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles = undefined
