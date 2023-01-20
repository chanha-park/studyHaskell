{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module OptParse (
    Options (..),
    SingleInput (..),
    SingleOutput (..),
    ReplaceFlag,
    parse,
) where

import Data.Maybe (fromMaybe)
import Options.Applicative

data Options
    = ConvertSingle SingleInput SingleOutput ReplaceFlag
    | ConvertDir FilePath FilePath ReplaceFlag
    deriving (Show)

data SingleInput
    = Stdin
    | InputFile FilePath
    deriving (Show)

data SingleOutput
    = Stdout
    | OutputFile FilePath
    deriving (Show)

type ReplaceFlag = Bool

-- inp :: Parser FilePath
-- inp =
--     strOption
--         ( long "input"
--             <> short 'i'
--             <> metavar "FILE"
--             <> help "Input File"
--         )

-- out :: Parser FilePath
-- out =
--     strOption
--         ( long "output"
--             <> short 'o'
--             <> metavar "FILE"
--             <> help "Output File"
--         )

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
        strOption
            ( long "input"
                <> short 'i'
                <> metavar "FILE"
                <> help "Input File"
            )

pOutputFile :: Parser SingleOutput
pOutputFile = fmap OutputFile parser
  where
    parser =
        strOption
            ( long "output"
                <> short 'o'
                <> metavar "FILE"
                <> help "Output File"
            )

pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe Stdout <$> optional pOutputFile

pReplaceFile :: Parser ReplaceFlag
pReplaceFile = switch (long "replace" <> short 'r' <> help "Replace existing file")

pConvertSingle :: Parser Options
pConvertSingle = ConvertSingle <$> pSingleInput <*> pSingleOutput <*> pReplaceFile

pInputDir :: Parser FilePath
pInputDir =
    strOption
        ( long "input"
            <> short 'i'
            <> metavar "DIRECTORY"
            <> help "Input Dir"
        )

pOutputDir :: Parser FilePath
pOutputDir =
    strOption
        ( long "output"
            <> short 'o'
            <> metavar "DIRECTORY"
            <> help "Output Dir"
        )

pConvertDir :: Parser Options
pConvertDir = ConvertDir <$> pInputDir <*> pOutputDir <*> pReplaceFile

-- subparser :: Mod CommandFields a -> Parser a
-- subparser = undefined

-- command :: String -> ParserInfo a -> Mod CommandFields a
-- command = undefined

-- info :: Parser a -> InfoMod a -> ParserInfo a
-- info = undefined

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
    info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand = command "convert" pConvertSingleInfo

pConvertDirInfo :: ParserInfo Options
pConvertDirInfo =
    info
        (helper <*> pConvertDir)
        (progDesc "Convert files in a directory to html")

pConvertDirCommand :: Mod CommandFields Options
pConvertDirCommand = command "convert-dir" pConvertDirInfo

pOptions :: Parser Options
pOptions = subparser (pConvertSingleCommand <> pConvertDirCommand)

opts :: ParserInfo Options
opts =
    info
        (helper <*> pOptions)
        ( fullDesc
            <> header "hs-blog-gen - a static blog generator"
            <> progDesc "Convert Markup file or dir to html"
        )

parse :: IO Options
parse = execParser opts
