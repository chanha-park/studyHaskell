{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

import Control.Exception
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import System.IO

data MyException = ErrZero | ErrOdd Int
    deriving (Show)

instance Exception MyException

sayDiv2 :: Int -> IO ()
sayDiv2 n
    | n == 0 = throwIO ErrZero
    | odd n = throwIO (ErrOdd n)
    | otherwise = print (div n 2)

main :: IO ()
main =
    hClose stdout
        >> catches
            ( putStrLn "Going to print a number now"
                >> getLine
                >>= sayDiv2 . read
                >> putStrLn "Did you like it?"
            )
            [ Handler $ \case
                ErrZero -> hPutStrLn stderr "Error : n == zero."
                ErrOdd n -> hPutStrLn stderr ("Error: " <> show n <> " is odd ")
            , Handler $ \(e :: IOException) ->
                if ioe_handle e /= Just stderr && ioe_type e /= IllegalOperation
                    then pure ()
                    else hPutStrLn stderr (displayException e)
            , Handler $ \(SomeException e) -> hPrint stderr e
            ]

-- main =
--     catch
--         ( putStrLn "Going to print a number now"
--             >> getLine
--             >>= sayDiv2 . read
--             >> putStrLn "Did you like it?"
--         )
--         ( \case
--             ErrZero -> hPutStrLn stderr "Error : n == zero."
--             ErrOdd n -> hPutStrLn stderr ("Error: " <> show n <> " is odd ")
--         )
