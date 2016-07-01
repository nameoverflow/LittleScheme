module Hscheme.Error (
    LispError(..),
    ThrowsError,
    throwError,
    extractValue,
    trapError
                     ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)
import Data.Char ()

import Hscheme.Parser

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (UnboundVar msg var) = msg ++ ": " ++ var
    show (BadSpecialForm msg form) = msg ++ ": " ++ show form
    show (NotFunction msg func) = msg ++ ": " ++ show func
    show (NumArgs expr found) = "Expected " ++ show expr ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expr found) = "Invalid type: expected " ++ expr ++ "; found" ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
