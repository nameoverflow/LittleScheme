module Hscheme.Environment (
    Env,
    getVar,
    setVar,
    defineVar,
    bindVars,
    newScope
) where

import Control.Applicative
import qualified Data.Map.Strict as M
import Data.IORef
import Control.Monad.Trans

import Hscheme.Types

lookupEnv :: String -> [Binds] -> Maybe (IORef LispVal)
lookupEnv var (cur:parent) =
    case M.lookup var cur of
      val@(Just _) -> val
      Nothing -> lookupEnv var parent

lookupEnv _ [] = Nothing

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
        writeIORef envRef (M.insert var valueRef curScope : parentScope)
    return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
  where
    extendEnv (cur:pre) = (: pre) <$> unionScope cur
    unionScope old = fmap (M.union old . M.fromList) (mapM addBindings bindings)
    addBindings (key, val) = newIORef val >>= \ref -> return (key, ref)

newScope :: Env -> IO Env
newScope old = readIORef old >>= \ref -> newIORef $ M.empty : ref
