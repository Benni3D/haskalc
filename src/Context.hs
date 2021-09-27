module Context where
import Control.Monad.State
import Expr

type Variable = (String, Expr)
type CalcEnv = [Variable]
type CalcContext = State CalcEnv

envAddVar :: (String, Expr) -> CalcContext ()
envAddVar v = do
   env <- get
   put $ v : env
   return ()

envGetVar :: String -> CalcContext Expr
envGetVar name = do
   env <- get
   return $ internal_findVar name env





internal_findVar :: String -> CalcEnv -> Expr
internal_findVar name []      = Error $ "variable " ++ name ++ " not found"
internal_findVar name (x:xs)  | name == (fst x)    = snd x
                              | otherwise          = internal_findVar name xs
