module Hscheme.Types (
    LispVal(..),
    LispFunc(..),
    LispError(..),
    ThrowsError,
    IOThrowsError,
    Binds,
    Env,
    nullEnv,
    throwError,
    extractValue,
    trapError,
    liftThrows,
    runIOThrows
) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)
import Data.Char ()
import Data.IORef
-- import qualified Data.HashTable as H
import qualified Data.Map.Strict as M

{-

  Basic types

 -}
data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Float Double
    | String String
    | Bool Bool
    | PrimitiveFun ([LispVal] -> IOThrowsError LispVal)
    | Fun LispFunc

data LispFunc = LispFunc {
    params :: [String],
    vararg :: Maybe String,
    body :: [LispVal],
    closure :: Env
}

instance Show LispVal where
    show (Atom name) = name
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
    show (String ctns) = "\"" ++ ctns ++ "\""
    show (Number num) = show num
    show (Float num) = show num
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (PrimitiveFun _) = "<primitive>"
    show (Fun LispFunc { params = args, vararg = varargs }) =
      "(lambda (" ++ unwords (map show args) ++ (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

{-
   Environments
-}

type Binds = M.Map String (IORef LispVal)
type Env = IORef [Binds]

nullEnv :: IO Env
nullEnv = newIORef [M.empty]

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction LispVal
               | UnboundVar String String
               | Default String
               | DupBound LispVal

instance Show LispError where
    show (UnboundVar msg var) = msg ++ ": " ++ var
    show (BadSpecialForm msg form) = msg ++ ": " ++ show form
    show (NotFunction func) = show func ++ " isn't a function"
    show (NumArgs expr found) = "Expected " ++ show expr ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expr found) = "Invalid type: expected " ++ expr ++ "; found" ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr
    show (DupBound var) = "Duplicate definition" ++ show var
    show (Default msg) = msg

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = fmap extractValue (runErrorT $ trapError action)
