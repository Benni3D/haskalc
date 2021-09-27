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
import Parsing
import Number

evalExpr :: Expr ->  Number
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

evalExpr (FCall "asin" [x])   = sin $ evalExpr x
evalExpr (FCall "asin" _)     = IVal 0

evalExpr (FCall "acos" [x])   = cos $ evalExpr x
evalExpr (FCall "acos" _)     = IVal 0

evalExpr (FCall "atan" [x])   = tan $ evalExpr x
evalExpr (FCall "atan" _)     = IVal 0

evalExpr (FCall _ _)          = IVal 0

-- TODO: implement
evalExpr (Var "pi")           = FVal pi
evalExpr (Var name)           = IVal 0
