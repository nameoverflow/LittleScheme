{-# LANGUAGE RankNTypes #-}
module Hscheme.Numeric (
    add,
    sub,
    numericDiv,
    mult,
    intOp
) where

import Control.Monad

import Hscheme.Types

fold1M :: (Monad m) => (a -> a -> m a) -> [a] -> m a
fold1M f (x:xs) = foldM f x xs

intOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
intOp _ [] = throwError $ NumArgs 2 []
intOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
intOp op args = fmap (Number . foldl1 op) (mapM unpackNum args)
    where
        unpackNum :: LispVal -> ThrowsError Integer
        unpackNum (Number num) = return num
        unpackNum bad = throwError $ TypeMismatch "number" bad



numericOp :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
numericOp _ [] = throwError $ NumArgs 2 []
numericOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericOp op args = fold1M op args

add, sub, mult :: [LispVal] -> ThrowsError LispVal
add = numericOp lispAdd
sub = numericOp lispSub
mult = numericOp lispMult

lispAdd :: LispVal -> LispVal -> ThrowsError LispVal
lispAdd (Number alpha) (Number beta) = return . Number $ alpha + beta
lispAdd (Number alpha) (Float beta) = return . Float $ fromInteger alpha + beta
lispAdd (Float alpha) (Number beta) = return . Float $ alpha + fromInteger beta
lispAdd (Float alpha) (Float beta) = return . Float $ alpha + beta
lispAdd alpha beta = throwError $ TypeMismatch "not number" bad
  where bad = case alpha of
                Number _ -> beta
                Float _ -> beta
                _ -> alpha

lispMult :: LispVal -> LispVal -> ThrowsError LispVal
lispMult (Number alpha) (Number beta) = return . Number $ alpha * beta
lispMult (Number alpha) (Float beta) = return . Float $ fromInteger alpha * beta
lispMult (Float alpha) (Number beta) = return . Float $ alpha * fromInteger beta
lispMult (Float alpha) (Float beta) = return . Float $ alpha * beta
lispMult alpha beta = throwError $ TypeMismatch "not number" bad
  where bad = case alpha of
                Number _ -> beta
                Float _ -> beta
                _ -> alpha

lispSub :: LispVal -> LispVal -> ThrowsError LispVal
lispSub (Number alpha) (Number beta) = return . Number $ alpha - beta
lispSub (Number alpha) (Float beta) = return . Float $ fromInteger alpha - beta
lispSub (Float alpha) (Number beta) = return . Float $ alpha - fromInteger beta
lispSub (Float alpha) (Float beta) = return . Float $ alpha - beta
lispSub alpha beta = throwError $ TypeMismatch "not number" bad
  where bad = case alpha of
                Number _ -> beta
                Float _ -> beta
                _ -> alpha

numericDiv :: [LispVal] -> ThrowsError LispVal
numericDiv args = fmap (Float . foldl1 (/)) (mapM unpackNum args)
  where
    unpackNum :: LispVal -> ThrowsError Double
    unpackNum (Number alpha) = return $ fromInteger alpha
    unpackNum (Float alpha) = return alpha
    unpackNum bad = throwError $ TypeMismatch "not number" bad
