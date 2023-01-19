{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

module Main where

import qualified HsBlog (convertDirectory, convertSingle)
import qualified OptParse
import System.Directory
import System.Exit
import System.IO

main :: IO ()
main =
    OptParse.parse
        >>= ( \case
                OptParse.ConvertDir input output -> HsBlog.convertDirectory input output
                OptParse.ConvertSingle input output -> do
                    (title, inputHandle) <- case input of
                        OptParse.Stdin -> pure ("", stdin)
                        OptParse.InputFile path -> (,) path <$> openFile path ReadMode

                    outputHandle <- case output of
                        OptParse.Stdout -> pure stdout
                        OptParse.OutputFile path ->
                            doesFileExist path
                                >>= ( \case
                                        True -> confirm "are you sure? (y/n)"
                                        False -> pure True
                                    )
                                >>= \case
                                    True -> openFile path WriteMode
                                    False -> exitFailure

                    HsBlog.convertSingle title inputHandle outputHandle
                    hClose inputHandle
                    hClose outputHandle
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
