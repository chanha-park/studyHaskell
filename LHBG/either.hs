{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

type Error = String

type Token = Float
type AST = Integer
type TypedAST = Double

newtype ExceptT e m a = ExceptT (m (Either e a))

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT = undefined

bindExceptT :: Monad m => ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
bindExceptT mx f =
    ExceptT
        ( runExceptT mx >>= \case
            Left err -> pure (Left err)
            Right y -> runExceptT (f y)
        )

-- throwError :: e -> ExceptT e m a
-- throwError (Left err) = ExceptT $ pure err

-- lift :: m a -> ExceptT e m a


tokenize :: String -> Either Error [Token]
tokenize = undefined

parse :: [Token] -> Either Error AST
parse = undefined

typecheck :: AST -> Either Error TypedAST
typecheck = undefined

parseToken :: String -> Either Error AST
parseToken string = tokenize string >>= parse

compile :: String -> Either Error TypedAST
compile string = tokenize string >>= parse >>= typecheck
