module Hscheme.Evalution (eval) where

import Control.Monad.Error

import Hscheme.Value
import Hscheme.Error
import Hscheme.Numeric

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (function : params)) = mapM eval params >>= apply function
eval bad = throwError $ BadSpecialForm "Unrecognized special form" bad

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
             ("+", intOp (+)),
             ("-", intOp (-)),
             ("*", intOp (*)),
             ("/", intOp div),
             ("mod", intOp mod),
             ("quotient", intOp quot),
             ("remainder", intOp rem)
             ]

apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply (Atom func) params = maybe (throwError $ NotFunction "Unrecognized function args" func)
                                 ($ params)
                                 $ lookup func primitives
