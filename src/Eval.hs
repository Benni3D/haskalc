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

evalExpr :: Expr -> Number
evalExpr (Val n)              = n
evalExpr (Paren e)            = evalExpr e
evalExpr (Unary '+' e)        = evalExpr e
evalExpr (Unary '-' e)        = -(evalExpr e)
evalExpr (Binary l '+' r)     = (evalExpr l) + (evalExpr r)
evalExpr (Binary l '-' r)     = (evalExpr l) - (evalExpr r)
evalExpr (Binary l '*' r)     = (evalExpr l) * (evalExpr r)
evalExpr (Binary l '/' r)     = (evalExpr l) / (evalExpr r)
evalExpr (Binary l '%' r)     = rem (evalExpr l) (evalExpr r)
evalExpr (Binary l '^' r)     = (evalExpr l) ** (evalExpr r)

evalExpr (FCall "sin" [x])    = sin $ evalExpr x
evalExpr (FCall "sin" _)      = IVal 0

evalExpr (FCall "cos" [x])    = cos $ evalExpr x
evalExpr (FCall "cos" _)      = IVal 0

evalExpr (FCall "tan" [x])    = tan $ evalExpr x
evalExpr (FCall "tan" _)      = IVal 0

evalExpr (FCall "asin" [x])   = asin $ evalExpr x
evalExpr (FCall "asin" _)     = IVal 0

evalExpr (FCall "acos" [x])   = acos $ evalExpr x
evalExpr (FCall "acos" _)     = IVal 0

evalExpr (FCall "atan" [x])   = atan $ evalExpr x
evalExpr (FCall "atan" _)     = IVal 0

evalExpr (FCall "sinh" [x])    = sinh $ evalExpr x
evalExpr (FCall "sinh" _)      = IVal 0

evalExpr (FCall "cosh" [x])    = cosh $ evalExpr x
evalExpr (FCall "cosh" _)      = IVal 0

evalExpr (FCall "tanh" [x])    = tanh $ evalExpr x
evalExpr (FCall "tanh" _)      = IVal 0

evalExpr (FCall "asinh" [x])   = asinh $ evalExpr x
evalExpr (FCall "asinh" _)     = IVal 0

evalExpr (FCall "acosh" [x])   = acosh $ evalExpr x
evalExpr (FCall "acosh" _)     = IVal 0

evalExpr (FCall "atanh" [x])   = atanh $ evalExpr x
evalExpr (FCall "atanh" _)     = IVal 0

evalExpr (FCall _ _)          = IVal 0

-- TODO: implement
evalExpr (Var "pi")           = FVal pi
evalExpr (Var name)           = IVal 0
