{-# LANGUAGE RankNTypes #-}
module Hscheme.Numeric where

import Control.Monad

import Hscheme.Parser
import Hscheme.Types

fold1M :: (Monad m) => (a -> a -> m a) -> [a] -> m a
fold1M f (x:xs) = foldM f x xs

intOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
intOp _ [] = throwError $ NumArgs 2 []
intOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
intOp op args = mapM unpackNum args >>= return . Number . foldl1 op
    where
        unpackNum :: LispVal -> ThrowsError Integer
        unpackNum (Number num) = return num
        unpackNum bad = throwError $ TypeMismatch "number" bad
{-
numericOp :: (Num a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numericOp _ [] = throwError $ NumArgs 2 []
numericOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericOp op args = fold1M doOp args
  where
    doOp :: LispVal -> LispVal -> ThrowsError LispVal
    doOp (Number alpha) (Number beta) = return . Number $ alpha `op` beta
    doOp (Number alpha) (Float beta) = return . Float $ (fromInteger alpha) `op` beta
    doOp (Float alpha) (Number beta) = return . Float $ alpha `op` (fromInteger beta)
    doOp (Float alpha) (Float beta) = return . Float $ alpha `op` beta
    doOp alpha beta = throwError $ TypeMismatch "number or float" bad
      where bad = case alpha of
                    Number _ -> beta
                    Float _ -> beta
                    _ -> alpha
-}
numericDiv :: [LispVal] -> ThrowsError LispVal
numericDiv args = mapM unpackNum args >>= return . Float . foldl1 (/)
  where
    unpackNum :: LispVal -> ThrowsError Double
    unpackNum (Number alpha) = return $ fromInteger alpha
    unpackNum (Float alpha) = return alpha
    unpackNum bad = throwError $ TypeMismatch "number or float" bad

