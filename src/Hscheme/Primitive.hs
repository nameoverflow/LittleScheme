module Hscheme.Primitive (
    primitives
) where

import System.IO
import Control.Arrow

import Hscheme.Types
import Hscheme.Environment

import qualified Hscheme.Numeric as N
import qualified Hscheme.IO as I

numericPrimitives :: [(String, LispVal)]
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

ioPrimitives :: [(String, LispVal)]
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

primitives :: IO Env
primitives = nullEnv >>= flip bindVars (numericPrimitives ++ ioPrimitives)
