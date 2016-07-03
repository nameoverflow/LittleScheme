module Hscheme.Value (
    LispVal(..)
) where

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Float Double
    | String String
    | Bool Bool
    | PrimitiveFun ([LispVal] -> ThrowsError LispVal)
    | Fun {
        params:: [String],
        vararg:: (Maybe String),
        body:: [LispVal],
        closure:: Env
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


unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
