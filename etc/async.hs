{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

import Control.Concurrent
import Control.Concurrent.Async

action1 :: IO Int
action1 = threadDelay 500000 >> return 5

action2 :: IO String
action2 = threadDelay 1000000 >> return "action2 result"

main :: IO ()
main =
  concurrently action1 action2 >>= \res ->
    print (res :: (Int, String))
      >> race action1 action2 >>= \res2 ->
        print (res2 :: Either Int String)
