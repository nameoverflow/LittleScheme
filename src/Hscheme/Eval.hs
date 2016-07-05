module Hscheme.Eval (eval) where

import Control.Monad.Error

import Hscheme.Types
-- import Hscheme.Numeric
import Hscheme.Environment

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ (List [Atom "quote", val]) = return val
eval env (Atom symbolName) = getVar env symbolName

eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : funcParams) : funcBody)) =
     makeNormalFunc env funcParams funcBody >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body

eval env (List (func : args)) = mapM (eval env) args >>= apply env func
eval _ bad = throwError $ BadSpecialForm "Unrecognized special form" bad

{-
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
             ("+", numericOp (+)),
             ("-", numericOp (-)),
             ("*", numericOp (*)),
             ("/", numericDiv),
             ("div", intOp div),
             ("mod", intOp mod),
             ("quotient", intOp quot),
             ("remainder", intOp rem)
             ]
-}

makeFunc :: (Maybe String) -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Fun (map show params) varargs body env

makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show

apply :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
-- apply env (Atom func) params = maybe (throwError $ NotFunction "Unrecognized function args" func)
--                                      ($ params)
--                                      $ getVar env func
apply env func args = do
    lambda <- eval env func
    case lambda of
      (Fun params varargs body closure) ->
        if num params /= num args && varargs == Nothing then
          throwError $ NumArgs (num params) args
        else do
          newEnv <- liftIO $ newScope closure >>= (flip bindVars $ zip params args) >>= bindVarArgs varargs
          liftM last $ mapM (eval newEnv) body
          where
             remainingArgs = drop (length params) args
             num  = toInteger . length
             bindVarArgs arg env = case arg of
               Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
               Nothing -> return env
      bad -> throwError $ NotFunction bad

