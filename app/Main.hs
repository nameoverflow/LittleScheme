module Main where

import System.Environment
import System.IO

import Control.Monad

import Hscheme.Evaluate

import Hscheme.Primitive


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
    args <- getArgs
    if null args
        then runRepl
        else primitives >>= flip runOne args
