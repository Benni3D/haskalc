module Context where
import Control.Monad.State
import Number

type Variable = (String, Number)
type EvalEnv = [Variable]
type EvalContext = State EvalEnv

envAddVar :: Variable -> EvalContext Number
envAddVar v = do
   env <- get
   put $ internal_addVar v env
   return (snd v)

envGetVar :: String -> EvalContext (Maybe Number)
envGetVar name = do
   env <- get
   return $ internal_findVar name env





internal_findVar :: String -> EvalEnv -> Maybe Number
internal_findVar name []                     = Nothing
internal_findVar name ((n,v):xs) | n == name = Just v
                                 | otherwise = internal_findVar name xs

internal_addVar :: Variable -> EvalEnv -> EvalEnv
internal_addVar v []                   = [v]
internal_addVar (n, v) ((en, ev):xs)   | n == en   = (n, v) : xs
                                       | otherwise = internal_addVar (n, v) xs
