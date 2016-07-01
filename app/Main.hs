module Main where

import System.Environment
import Control.Monad
import Hscheme.Parser
import Hscheme.Error
import Hscheme.Evalution

readExpr :: String -> ThrowsError LispVal
readExpr input = case parsed of
    Left err -> throwError $ Parser err
    Right val -> return val
    where
        parsed = parse parseExpr "lisp" input

run :: String -> IO()
run expr = do
    result <- return $ liftM show $ readExpr expr >>= eval
    putStrLn $ extractValue $ trapError result

main :: IO ()
main = do
    (expr:_) <- getArgs
    run expr
