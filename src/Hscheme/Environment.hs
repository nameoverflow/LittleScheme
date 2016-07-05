module Hscheme.Environment (
    Env,
    getVar,
    setVar,
    defineVar,
    bindVars,
    newScope
) where

import qualified Data.Map.Strict as M
import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Monad.Trans

import Hscheme.Types

lookupEnv :: String -> [Binds] -> Maybe (IORef LispVal)
lookupEnv var (cur:parent) =
    case M.lookup var cur of
      val@(Just _) -> val
      Nothing -> lookupEnv var parent

lookupEnv _ [] = Nothing

lookupCur :: String -> [Binds] -> Maybe (IORef LispVal)
lookupCur var (ref:_) = M.lookup var ref
lookupCur _ [] = Nothing

isBound :: Env -> String -> IO Bool
isBound env var = readIORef env >>= return . isJust . lookupEnv var

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
          (liftIO . flip writeIORef val)
          (lookupEnv var env)
    return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
    liftIO $ do
        valueRef <- newIORef val
        (curScope:parentScope) <- readIORef envRef
        writeIORef envRef ((M.insert var valueRef curScope):parentScope)
    return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
  where
    extendEnv (cur:pre) = liftM (:pre) $ unionScope cur
    unionScope old = mapM addBindings bindings >>= return . (M.union old) . M.fromList
    addBindings (key, val) = newIORef val >>= \ref -> return (key, ref)

newScope :: Env -> IO Env
newScope old = readIORef old >>= \ref -> newIORef $ M.empty : ref
