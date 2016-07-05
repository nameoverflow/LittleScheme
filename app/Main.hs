module Main where

import System.Environment
import Control.Monad
import System.IO

import Hscheme.Parser
import Hscheme.Types
import Hscheme.Eval

readExpr :: String -> ThrowsError LispVal
readExpr input = case parsed of
    Left err -> throwError $ Parser err
    Right val -> return val
    where
        parsed = parse parseExpr "lisp" input

run :: Env -> String -> IO String
run env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runAndPrint :: Env -> String -> IO ()
runAndPrint env expr = run env expr >>= putStrLn

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . runAndPrint

main :: IO ()
main = do
    (expr:_) <- getArgs
    env <- nullEnv
    runAndPrint env expr
