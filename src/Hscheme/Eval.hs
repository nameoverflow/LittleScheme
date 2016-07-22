module Hscheme.Eval (
    eval
) where

    import Data.Maybe
    import Control.Monad.Error
    import Control.Applicative
    import Hscheme.Types
    import qualified Hscheme.Numeric as N
    import Hscheme.Environment

    eval :: Env -> LispVal -> IOThrowsError LispVal
    eval _ val@(String _) = return val
    eval _ val@(Number _) = return val
    eval _ val@(Float _) = return val
    eval _ val@(Bool _) = return val
    eval _ (List [Atom "quote", val]) = return val
    eval env (List [Atom "unquote", val]) = eval env val
    eval env (Atom symbolName) = getVar env symbolName

    eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

    eval env (List (Atom "define" : List (Atom var : funcParams) : funcBody)) =
         makeNormalFunc env funcParams funcBody >>= defineVar env var
    eval env (List (Atom "define" : DottedList (Atom var : args) varargs : body)) =
         makeVarArgs varargs env args body >>= defineVar env var

    eval env (List (Atom "lambda" : List args : body)) =
         makeNormalFunc env args body
    eval env (List (Atom "lambda" : DottedList params varargs : body)) =
         makeVarArgs varargs env params body
    eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
         makeVarArgs varargs env [] body

    eval env (List (func : args)) = mapM (eval env) args >>= apply env func
    eval _ bad = throwError $ BadSpecialForm "Unrecognized special form" bad


    makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
    makeFunc varargs env params body = return . Fun $ LispFunc (map show params) varargs body env

    makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
    makeNormalFunc = makeFunc Nothing
    makeVarArgs = makeFunc . Just . show

    apply :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
    -- apply env (Atom func) params = maybe (throwError $ NotFunction "Unrecognized function args" func)
    --                                      ($ params)
    --                                      $ getVar env func
    apply env func args = do
        lambda <- eval env func
        case lambda of
          (Fun (LispFunc params varargs body closure)) ->
            if num params /= num args && isNothing varargs then
              throwError $ NumArgs (num params) args
            else do
              newEnv <- liftIO $ newScope closure >>= flip bindVars (zip params args) >>= bindVarArgs varargs
              last <$> mapM (eval newEnv) body
              where
                 remainingArgs = drop (length params) args
                 num  = toInteger . length
                 bindVarArgs arg env = case arg of
                   Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                   Nothing -> return env

          (PrimitiveFun pri) -> pri args

          bad -> throwError $ NotFunction bad
