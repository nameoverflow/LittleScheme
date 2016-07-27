module Main where

import System.Environment
import System.IO

import Control.Monad

import Hscheme.Parser
import Hscheme.Types
import Hscheme.Evaluate

import Hscheme.Primitive

readExpr :: String -> ThrowsError LispVal
readExpr input = case parsed of
    Left err -> throwError $ Parser err
    Right val -> return val
    where
        parsed = parse parseExpr "lisp" input

run :: Env -> String -> IO String
run env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

runAndPrint :: Env -> String -> IO ()
runAndPrint env expr = run env expr >>= putStrLn

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ condi prompt action = do
   result <- prompt
   unless (condi result) $ action result >> until_ condi prompt action

runRepl :: IO ()
runRepl = primitives >>= until_ (== "quit") (readPrompt "λ > ") . runAndPrint

main :: IO ()
main = do
    (expr:_) <- getArgs
    env <- primitives
    runAndPrint env expr
