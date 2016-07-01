module Hscheme.Numeric where

import Hscheme.Parser
import Hscheme.Error

{-

universalDiv :: [LispVal] -> ThrowsError LispVal
universalDiv args =
    let op = numOp (/) in
        case args of
          emp@[] -> op emp
          single@[_] -> op single
          ((Number x):xs) -> op $ (Float . fromInteger $ x) : xs

numOp :: (Num a) => (a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numOp op [] = throwError $ NumArgs 2 []
numOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numOp op args = return $ foldl1 alignTypedOp args
    where
        alignTypedOp :: LispVal -> LispVal -> LispVal
        alignTypedOp (Number l) (Float r) = Float $ fromInteger l  `op` r
        alignTypedOp (Float l) (Number r) = Float $ l `op` fromInteger r
        alignTypedOp (Float l) (Float r) = Float $ l `op` r
        alignTypedOp (Number l) (Number r) = Number $ l `op` r

-}

intOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
intOp _ [] = throwError $ NumArgs 2 []
intOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
intOp op args = mapM unpackNum args >>= return . Number . foldl1 op
    where
        unpackNum :: LispVal -> ThrowsError Integer
        unpackNum (Number num) = return num
        unpackNum bad = throwError $ TypeMismatch "number" bad

data TheList a = Cons a (TheList a) | Nil

theCons :: a -> TheList a -> TheList a
theCons a l = Cons a l

theHead (Cons h _) = h

theTail (Cons _ t) = t
