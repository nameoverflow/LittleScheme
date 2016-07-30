module Hscheme.Evaluate (
    runOne,
    runAndPrint
) where

import Data.Maybe

import System.IO
import Control.Monad
import Control.Monad.Error
import Control.Applicative

import Hscheme.IO (load)
import Hscheme.Types
import Hscheme.Parser
import Hscheme.Environment



-- readExpr :: String -> ThrowsError LispVal
-- readExpr input = case parsed of
--     Left err -> throwError $ Parser err
--     Right val -> return val
--     where
--         parsed = parse parseExpr "lisp" input

run :: Env -> String -> IO String
run env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env (makeNullCont env)

runAndPrint :: Env -> String -> IO ()
runAndPrint env expr = run env expr >>= putStrLn

runOne :: Env -> [String] -> IO ()
runOne env' args = do
    env <- bindVars env' [("args", List $ map String $ drop 1 args)]
    runIOThrows (show <$> eval env (makeNullCont env) (List [Atom "load", String $ head args]))
        >>= hPutStrLn stderr



eval :: Env -> Continuation -> LispVal -> IOThrowsError LispVal

-- literal
eval _ (Continuation _ cBody) val@(String _) = cBody val
eval _ (Continuation _ cBody) val@(Number _) = cBody val
eval _ (Continuation _ cBody) val@(Float _) = cBody val
eval _ (Continuation _ cBody) val@(Bool _) = cBody val
eval _ (Continuation _ cBody) val@(Cont _) = cBody val
-- list
eval _ (Continuation _ cBody) (List [Atom "quote", val]) = cBody val
-- eval env (List [Atom "unquote", val]) = eval env val

-- symbol getting
eval env (Continuation _ cBody) (Atom symbolName) = getVar env symbolName >>= cBody


-- binding definition
eval env cont@(Continuation _ cBody) (List [Atom "define", Atom var, form]) = eval env cont form >>= defineVar env var >>= cBody

-- function definition
eval env (Continuation _ cBody) (List (Atom "define" : List (Atom var : funcParams) : funcBody)) =
     makeNormalFunc env funcParams funcBody >>= defineVar env var >>= cBody
eval env (Continuation _ cBody) (List (Atom "define" : DottedList (Atom var : args) varargs : funcBody)) =
     makeVarArgs varargs env args funcBody >>= defineVar env var >>= cBody

-- lambda expression
eval env (Continuation _ cBody) (List (Atom "lambda" : List args : funcBody)) =
     makeNormalFunc env args funcBody >>= cBody
eval env (Continuation _ cBody) (List (Atom "lambda" : DottedList args varargs : funcBody)) =
     makeVarArgs varargs env args funcBody >>= cBody
eval env (Continuation _ cBody) (List (Atom "lambda" : varargs@(Atom _) : funcBody)) =
     makeVarArgs varargs env [] funcBody >>= cBody

-- load
eval env cont (List [Atom "load", String filename]) =
    load filename >>= fmap last . mapM (eval env cont)

-- set!
eval env (Continuation _ cBody) (List [Atom "set!", Atom var, val]) = setVar env var val >>= cBody

-- call/cc
eval env cont (List [Atom "call-with-current-continuation", func]) =
    applyWithCont env cont func [Cont cont]

eval _ _ (List [Cont cont@(Continuation cEnv _), val]) = eval cEnv cont val

-- if
eval env cont (List [Atom "if", predl, conseq, alt]) =
    eval env (Continuation env cont') predl
    where
        cont' res = case res of
            Bool False -> eval env cont alt
            _          -> eval env cont conseq

-- ordinary function appling
eval env cont (List (func : args)) = applyWithCont env cont func args

-- unhandled form
eval _ _ bad = throwError $ BadSpecialForm "Unrecognized special form" bad

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env args funcBody = return . Fun $ LispFunc (map show args) varargs funcBody env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show

runSeqAsCont :: Env -> Continuation -> [LispVal] -> IOThrowsError LispVal
runSeqAsCont env cont@(Continuation _ cBody) lispSeq =
    case lispSeq of

        [] -> throwError $ Default "Empty function body"

        [lisp] ->
            eval env (Continuation env cBody) lisp

        (lispHead:lispRest) ->
            eval env (Continuation env (\_ -> runSeqAsCont env cont lispRest)) lispHead


applyWithCont :: Env -> Continuation -> LispVal -> [LispVal] -> IOThrowsError LispVal
applyWithCont env cont@(Continuation _ cBody) func args' = eval env (Continuation env runFunction) func

    where
        evalArgs = makeArgsCont env args' []
        num  = toInteger . length

        makeFuncCont :: LispFunc -> [LispVal] -> IOThrowsError LispVal
        makeFuncCont (LispFunc funcParams varargs funcBody funcClosure) args =
            if num funcParams /= num args && isNothing varargs
                then throwError $ NumArgs (num funcParams) args
                else do
                    newEnv <- liftIO $ newScope funcClosure
                                >>= flip bindVars (zip funcParams args)
                                >>= bindVarArgs varargs
                    runSeqAsCont newEnv cont funcBody
            where
                remainingArgs = drop (length funcParams)
                bindVarArgs arg bindEnv = case arg of
                    Just argName ->
                        liftIO $ bindVars bindEnv [(argName, List $ remainingArgs args)]
                    Nothing -> return bindEnv


        runFunction :: LispVal -> IOThrowsError LispVal
        runFunction lambda = case lambda of
            (Fun lispFunc) ->
                evalArgs $ makeFuncCont lispFunc

            (PrimitiveFun pri) ->
                evalArgs $ liftThrows . pri >=> cBody

            (Cont contVal@(Continuation cEnv _)) ->
                case args' of
                    [val] -> eval cEnv contVal val
                    _     -> throwError $ NumArgs 1 args'

            val -> throwError $ NotFunction val

makeArgsCont :: Env -> [LispVal] -> [LispVal]
                -> ([LispVal] -> IOThrowsError LispVal)
                -> IOThrowsError LispVal

makeArgsCont _ [] result contFunc = contFunc result

makeArgsCont env (arg:restArgs) results func = eval env (Continuation env evalRest) arg
    where
        evalRest result' = makeArgsCont env restArgs (result':results) func

-- callWithCurrentCont :: Env -> Continuation -> LispVal -> IOThrowsError LispVal
-- callWithCurrentCont env cont@(Continuation cEnv cBody) fun = eval env currentCont fun
--     where
--         currentCont resFun =
