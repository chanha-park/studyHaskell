{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

-- import Control.Monad.Reader
-- import System.Exit
-- import System.Process

-- type ExitStatus = Int

-- runCmd :: String -> ReaderT ExitStatus IO ExitStatus
-- runCmd cmd = do
--     -- prevExitStatus <- ask
--     exitCode <- liftIO $ system cmd
--     case exitCode of
--         ExitSuccess -> return 0
--         ExitFailure n -> return n

-- main :: IO ()
-- main = myLoop 0

-- myLoop :: Int -> IO ()
-- myLoop x = do
--     cmd <- getLine
--     exitCode <- runReaderT (runCmd cmd) x
--     case exitCode of
--         0 -> putStrLn "Command succeeded"
--         n -> putStrLn $ "Command failed with code: " ++ show n
--     myLoop exitCode

import Control.Monad.Reader
import System.Environment
import System.Exit
import System.Process

type ExitStatus = Int
type Env = [(String, String)]

runCmd :: String -> ReaderT (ExitStatus, Env) IO (ExitStatus, Env)
runCmd cmd = do
    -- (prevExitStatus, env) <- ask
    -- it's not modified env. Have to parse cmd to determine it's export or set, etc
    -- which affects env
    exitCode <- liftIO $ system cmd
    newEnv <- liftIO getEnvironment
    case exitCode of
        ExitSuccess -> return (0, newEnv)
        ExitFailure n -> return (n, newEnv)

main :: IO ()
main = do
    initEnv <- getEnvironment
    myLoop 0 initEnv

myLoop :: Int -> Env -> IO ()
myLoop x y = do
    cmd <- getLine
    (exitCode, env) <- runReaderT (runCmd cmd) (x, y)
    case exitCode of
        0 -> putStrLn "Command succeeded"
        n -> putStrLn $ "Command failed with code: " ++ show n
    -- putStrLn $ "Current environment: " ++ show env
    myLoop exitCode env
