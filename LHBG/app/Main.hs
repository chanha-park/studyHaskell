{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module Main where

-- import Control.Exception
import qualified HsBlog (convertDirectory, convertSingle)
import qualified OptParse
import System.Directory
import System.Exit
import System.IO

main :: IO ()
main =
    OptParse.parse
        >>= ( \case
                OptParse.ConvertDir input output _ ->
                    HsBlog.convertDirectory input output
                OptParse.ConvertSingle input output replaceFlag ->
                    withInputHandle ((withOutputHandle .) . HsBlog.convertSingle)
                  where
                    withInputHandle :: (String -> Handle -> IO a) -> IO a
                    withInputHandle action = case input of
                        OptParse.Stdin -> action "" stdin
                        OptParse.InputFile path -> withFile path ReadMode (action path)

                    withOutputHandle :: (Handle -> IO b) -> IO b
                    withOutputHandle action = case output of
                        OptParse.Stdout -> action stdout
                        OptParse.OutputFile path ->
                            doesFileExist path
                                >>= ( \exist ->
                                        if not exist || replaceFlag
                                            then pure True
                                            else confirm "are you sure? (y/n)"
                                    )
                                >>= \case
                                    True -> withFile path WriteMode action
                                    False -> exitFailure
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
