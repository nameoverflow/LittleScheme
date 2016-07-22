module Hscheme.Primitive (
    primitives
) where

    import Control.Arrow

    import Hscheme.Types
    import qualified Hscheme.Numeric as N
    import Hscheme.Environment

    primitivesList :: [(String, LispVal)]
    primitivesList = map (second PrimitiveFun) [
                 ("+", N.add),
                 ("-", N.sub),
                 ("*", N.mult),
                 ("/", N.numericDiv),
                 ("div", N.intOp div),
                 ("mod", N.intOp mod),
                 ("quotient", N.intOp quot),
                 ("remainder", N.intOp rem)
                 ]

    primitives :: IO Env
    primitives = nullEnv >>= flip bindVars primitivesList
