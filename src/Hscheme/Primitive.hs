module Hscheme.Primitive (
    primitives
) where

import System.IO
import Control.Arrow
import Control.Applicative


import Hscheme.Types
import Hscheme.Environment

import qualified Hscheme.Numeric as N
import qualified Hscheme.IO as I

primitives :: IO Env
primitives = nullEnv >>= flip bindVars (numericPrimitives ++ boolPrimitives ++ ioPrimitives)

numericPrimitives, boolPrimitives, ioPrimitives :: [(String, LispVal)]
numericPrimitives = map (second PrimitiveFun) [
        ("+", N.add),
        ("-", N.sub),
        ("*", N.mult),
        ("/", N.numericDiv),
        ("div", N.intOp div),
        ("mod", N.intOp mod),
        ("quotient", N.intOp quot),
        ("remainder", N.intOp rem)
    ]

boolPrimitives = map (second PrimitiveFun) [
        ("=", numBoolBinop (==)),
        ("<", numBoolBinop (<)),
        (">", numBoolBinop (>)),
        ("/=", numBoolBinop (/=)),
        (">=", numBoolBinop (>=)),
        ("<=", numBoolBinop (<=)),
        ("&&", boolBoolBinop (&&)),
        ("||", boolBoolBinop (||)),
        ("string=?", strBoolBinop (==)),
        ("string<?", strBoolBinop (<)),
        ("string>?", strBoolBinop (>)),
        ("string<=?", strBoolBinop (<=)),
        ("string>=?", strBoolBinop (>=))
    ]

ioPrimitives = map (second IOFun) [
        ("apply", I.applyProc),
        ("open-input-file", I.makePort ReadMode),
        ("open-output-file", I.makePort WriteMode),
        ("close-input-port", I.closePort),
        ("close-output-port", I.closePort),
        ("read", I.readProc),
        ("write", I.writeProc),
        ("read-contents", I.readContents),
        ("read-all", I.readAll)
    ]

boolOp :: Ord a => (LispVal -> ThrowsError a)
                   -> (a -> a -> Bool)
                   -> [LispVal] -> ThrowsError LispVal

boolOp unpack op [left, right] = Bool <$> liftA2 op (unpack left) (unpack right)
boolOp _ _ args = throwError $ NumArgs 2 args

numBoolBinop  = boolOp unpackNum
strBoolBinop  = boolOp unpackStr
boolBoolBinop = boolOp unpackBool

unpackNum :: LispVal -> ThrowsError Double
unpackNum (Number val) = return $ fromInteger val
unpackNum (Float val) = return val

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool
