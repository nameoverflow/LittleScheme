module Hscheme.Evaluate (
    eval
) where

import Data.Maybe
import Control.Monad.Error
import Control.Applicative
import Hscheme.Types
import Hscheme.Environment

eval :: Env -> LispVal -> IOThrowsError LispVal

-- literal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Float _) = return val
eval _ val@(Bool _) = return val

-- list
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "unquote", val]) = eval env val

-- symbol getting
eval env (Atom symbolName) = getVar env symbolName


-- binding definition
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

-- function definition
eval env (List (Atom "define" : List (Atom var : funcParams) : funcBody)) =
     makeNormalFunc env funcParams funcBody >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : args) varargs : funcBody)) =
     makeVarArgs varargs env args funcBody >>= defineVar env var

-- lambda expression
eval env (List (Atom "lambda" : List args : funcBody)) =
     makeNormalFunc env args funcBody
eval env (List (Atom "lambda" : DottedList args varargs : funcBody)) =
     makeVarArgs varargs env args funcBody
eval env (List (Atom "lambda" : varargs@(Atom _) : funcBody)) =
     makeVarArgs varargs env [] funcBody


-- apply expression
eval env (List (Atom "apply" : func : args)) = apply env func args


-- ordinary function appling
eval env (List (func : args)) = apply env func args

-- unhandled form
eval _ bad = throwError $ BadSpecialForm "Unrecognized special form" bad

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env args funcBody = return . Fun $ LispFunc (map show args) varargs funcBody env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show

apply :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply env func originArgs = do
    args <- mapM (eval env) originArgs
    lambda <- eval env func
    case lambda of
      (Fun (LispFunc funcParams varargs funcBody funcClosure)) ->
        if num funcParams /= num args && isNothing varargs then
          throwError $ NumArgs (num funcParams) args
        else do
          newEnv <- liftIO $ newScope funcClosure >>= flip bindVars (zip funcParams args) >>= bindVarArgs varargs
          last <$> mapM (eval newEnv) funcBody
          where
             remainingArgs = drop (length funcParams) args
             num  = toInteger . length
             bindVarArgs arg bindEnv = case arg of
               Just argName -> liftIO $ bindVars bindEnv [(argName, List remainingArgs)]
               Nothing -> return bindEnv

      (PrimitiveFun pri) -> pri args

      bad -> throwError $ NotFunction bad
