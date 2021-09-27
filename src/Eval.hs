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

evalExpr :: Expr -> EvalResult
evalExpr (Val n)              = Right n
evalExpr (Paren e)            = evalExpr e
evalExpr (Unary '+' e)        = evalExpr e
evalExpr (Unary '-' e)        = evalExpr e >>= (\x -> Right $ -x)
evalExpr (Binary l '+' r)     = do { x <- evalExpr l; y <- evalExpr r; Right $ x + y }
evalExpr (Binary l '-' r)     = do { x <- evalExpr l; y <- evalExpr r; Right $ x - y }
evalExpr (Binary l '*' r)     = do { x <- evalExpr l; y <- evalExpr r; Right $ x * y }
evalExpr (Binary l '/' r)     = do { x <- evalExpr l; y <- evalExpr r; Right $ x / y }
evalExpr (Binary l '%' r)     = do { x <- evalExpr l; y <- evalExpr r; Right $ x `rem` y }
evalExpr (Binary l '^' r)     = do { x <- evalExpr l; y <- evalExpr r; Right $ x ** y }
evalExpr (Error msg)          = Left msg

evalExpr (FCall "sin" [mx])   = do { x <- evalExpr mx; Right $ sin x }
evalExpr (FCall "sin" _)      = Left "sin(x): invalid argument count"

evalExpr (FCall "cos" [mx])   = do { x <- evalExpr mx; Right $ cos x }
evalExpr (FCall "cos" _)      = Left "cos(x): invalid argument count"

evalExpr (FCall "tan" [mx])   = do { x <- evalExpr mx; Right $ tan x }
evalExpr (FCall "tan" _)      = Left "tan(x): invalid argument count"

evalExpr (FCall "asin" [mx])  = do { x <- evalExpr mx; Right $ asin x }
evalExpr (FCall "asin" _)     = Left "asin(x): invalid argument count"

evalExpr (FCall "acos" [mx])  = do { x <- evalExpr mx; Right $ acos x }
evalExpr (FCall "acos" _)     = Left "acos(x): invalid argument count"

evalExpr (FCall "atan" [mx])  = do { x <- evalExpr mx; Right $ atan x }
evalExpr (FCall "atan" _)     = Left "atan(x): invalid argument count"

evalExpr (FCall "sinh" [mx])  = do { x <- evalExpr mx; Right $ sinh x }
evalExpr (FCall "sinh" _)     = Left "sinh(x): invalid argument count"

evalExpr (FCall "cosh" [mx])  = do { x <- evalExpr mx; Right $ cosh x }
evalExpr (FCall "cosh" _)     = Left "cosh(x): invalid argument count"

evalExpr (FCall "tanh" [mx])  = do { x <- evalExpr mx; Right $ tanh x }
evalExpr (FCall "tanh" _)     = Left "tanh(x): invalid argument count"

evalExpr (FCall "asinh" [mx]) = do { x <- evalExpr mx; Right $ asinh x }
evalExpr (FCall "asinh" _)     = Left "asinh(x): invalid argument count"

evalExpr (FCall "acosh" [mx]) = do { x <- evalExpr mx; Right $ acosh x }
evalExpr (FCall "acosh" _)     = Left "acosh(x): invalid argument count"

evalExpr (FCall "atanh" [mx]) = do { x <- evalExpr mx; Right $ atanh x }
evalExpr (FCall "atanh" _)     = Left "atanh(x): invalid argument count"

evalExpr (FCall _ _)          = Left "User-defined functions are not supported"

evalExpr (Var name)           = Left "Variables are not supported"

evalExpr e                    = Left $ "failed to evaluate `" ++ (show e) ++ "`"

