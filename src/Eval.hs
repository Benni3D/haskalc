{-
 -  Copyright (C) 2021 Benjamin Stürz
 -  
 -  This program is free software: you can redistribute it and/or modify
 -  it under the terms of the GNU General Public License as published by
 -  the Free Software Foundation, either version 3 of the License, or
 -  (at your option) any later version.
 -  
 -  This program is distributed in the hope that it will be useful,
 -  but WITHOUT ANY WARRANTY; without even the implied warranty of
 -  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -  GNU General Public License for more details.
 -  
 -  You should have received a copy of the GNU General Public License
 -  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 -}

module Eval where
import Context
import Number
import Expr

type EvalResult = Either [Char] Number

showEvalResult :: EvalResult -> [Char]
showEvalResult (Right r)      = show r
showEvalResult (Left msg)     = "Error: " ++ msg


do_binary :: (Number -> Number -> Number) -> Expr -> Expr -> EvalContext EvalResult
do_binary f l r = do
   mx <- evalExpr l
   my <- evalExpr r
   return $ mx >>= (\x -> my >>= (\y -> Right $ f x y))

do_unary_fcall :: String -> (Number -> Number) -> [Expr] -> EvalContext EvalResult
do_unary_fcall _ f [e] = do { mx <- evalExpr e; return $ mx >>= (\x -> Right $ f x) }
do_unary_fcall n _ _   = return $ Left $ n ++ "(x): invalid argument count"

evalExpr :: Expr -> EvalContext EvalResult
evalExpr (Error msg)          = return $ Left msg
evalExpr (Val n)              = return $ Right n
evalExpr (Paren e)            = evalExpr e
evalExpr (Unary '+' e)        = evalExpr e
evalExpr (Unary '-' e)        = do { mx <- evalExpr e; return $ mx >>= (\x -> Right (-x)) }
evalExpr (Binary l '+' r)     = do_binary (+) l r
evalExpr (Binary l '-' r)     = do_binary (-) l r
evalExpr (Binary l '*' r)     = do_binary (*) l r
evalExpr (Binary l '/' r)     = do_binary (/) l r
evalExpr (Binary l '%' r)     = do_binary rem l r
evalExpr (Binary l '^' r)     = do_binary (**) l r
evalExpr (Binary (Var name) '=' r) = do
                              mv <- evalExpr r
                              case mv of
                                 (Left msg)  -> return $ Left msg
                                 (Right v)   -> do
                                    envAddVar (name,v)
                                    return $ Right v
evalExpr (Binary l '=' _)     = return $ Left $ "cannot assign to `" ++ (show l) ++ "`"

evalExpr (FCall "sin" a)      = do_unary_fcall "sin" sin a
evalExpr (FCall "cos" a)      = do_unary_fcall "cos" cos a
evalExpr (FCall "tan" a)      = do_unary_fcall "tan" tan a
evalExpr (FCall "sinh" a)     = do_unary_fcall "sinh" sinh a
evalExpr (FCall "cosh" a)     = do_unary_fcall "cosh" cosh a
evalExpr (FCall "tanh" a)     = do_unary_fcall "tanh" tanh a
evalExpr (FCall "asin" a)     = do_unary_fcall "asin" asin a
evalExpr (FCall "acos" a)     = do_unary_fcall "acos" acos a
evalExpr (FCall "atan" a)     = do_unary_fcall "atan" atan a
evalExpr (FCall "asinh" a)    = do_unary_fcall "asinh" asinh a
evalExpr (FCall "acosh" a)    = do_unary_fcall "acosh" acosh a
evalExpr (FCall "atanh" a)    = do_unary_fcall "atanh" atanh a

evalExpr (FCall _ _)          = return $ Left "User-defined functions are not supported"

evalExpr (Var "pi")           = return $ Right $ FVal pi

evalExpr (Var name)           = do
   mv <- envGetVar name
   case mv of
      Nothing  -> return $ Left $ "Variable " ++ name ++ " does not exist."
      Just v   -> return $ Right v


evalExpr e                    = return $ Left $ "failed to evaluate `" ++ (show e) ++ "`"

