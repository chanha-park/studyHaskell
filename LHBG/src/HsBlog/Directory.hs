{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module HsBlog.Directory (
    convertDirectory,
    buildIndex,
) where

import HsBlog.Convert (convert, convertStructure)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

import Control.Monad (void, when)
import Data.Functor ((<&>))
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

data DirContents = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
    , dcFilesToCopy :: [FilePath]
    }

checkTxtExtension :: [FilePath] -> ([FilePath], [FilePath])
checkTxtExtension = partition ((== ".txt") . takeExtension)

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir =
    listDirectory inputDir
        >>= ( \(xs, ys) ->
                applyIOonTraversal readFile xs
                    >>= filterAndReportFailures
                    >>= ( \xs' ->
                            pure $
                                DirContents
                                    { dcFilesToProcess = xs'
                                    , dcFilesToCopy = ys
                                    }
                        )
            )
            . checkTxtExtension
            . map (inputDir </>)

applyIOonTraversal ::
    Traversable t =>
    (a -> IO b) ->
    t a ->
    IO (t (a, Either String b))
applyIOonTraversal fn xs =
    for
        xs
        ( \x ->
            catch
                (Right <$> fn x)
                (\(SomeException e) -> pure . Left . displayException $ e)
                <&> (,) x
        )

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures = foldMap fn
  where
    fn :: (a, Either String b) -> IO [(a, b)]
    fn (x, Right y) = pure [(x, y)]
    fn (_, Left y) = hPutStrLn stderr y >> pure []

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
createOutputDirectoryOrExit outputDir =
    createOutputDirectory outputDir
        >>= ( \case
                True -> pure ()
                False -> hPutStrLn stderr "Cancelled." >> exitFailure
            )
  where
    createOutputDirectory :: FilePath -> IO Bool
    createOutputDirectory dir =
        doesDirectoryExist dir
            >>= ( \case
                    True ->
                        confirm "already exist. override?"
                            >>= ( \x ->
                                    when x (removeDirectoryRecursive dir)
                                        >> pure x
                                )
                    False -> pure True
                )
            >>= (\x -> when x (createDirectory dir) >> pure x)

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

txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml = undefined

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile = undefined

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile = undefined

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles = undefined

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles = undefined
