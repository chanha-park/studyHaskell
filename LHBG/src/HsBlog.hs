{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

module HsBlog (
    convertSingle,
    convertDirectory,
    process,
)
where

import HsBlog.Convert (process)
import qualified HsBlog.Html as Html

import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = hGetContents input >>= hPutStrLn output . process title

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = undefined
