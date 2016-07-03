module Hscheme.Environment (
    Env(..)
) where

import qualified Data.HashTable as H
import Data.IORef
import Data.Maybe

import Hscheme.Value
import Hscheme.Error

type HashTable = H.HashTable String LispVal
type Env = IORef [HashTable]

nullEnv :: IO Env
nullEnv = newIORef Nothing

newSubEnv :: Env -> Env
newSubEnv ref = do
    p <- readIORef ref
    sub <- H.new (==) H.hashString
    return $ sub:p

lookupEnv :: String -> [HashTable] -> IO (Maybe LispVal)
lookupEnv var (cur:parent) = do
    res <- H.lookup var cur
    case res of
        val@(Just _) -> return val
        Nothing -> lookupEnv var parent
lookupEnv _ [] = return Nothing

lookupCur :: String -> [HashTable] -> IO (Maybe LispVal)
lookupCur var (ref:_) = H.lookup var ref

-- TODO: 返回的 IO 类型不符合
isBound :: Env -> String -> IO Bool
isBound env var = readIORef env >>= isJust . lookupEnv var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting a unbound varible" var)
          (liftIO . readIORef)
          (lookupEnv var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting a unbound varible" var)
          (liftIO . flip writeIORef value)
          (lookupEnv var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
    alreadyDefined <- liftIO $ lookupCur var envRef
    if alreadyDefined then
        throwError $ DupBound "Duplicate definition" var
    else
        liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef -- TODO
