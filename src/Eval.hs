module Eval where
import Parsing

evalExpr :: Expr -> Int
evalExpr (Val n)           = n
evalExpr (Paren e)         = evalExpr e
evalExpr (Unary '+' e)     = evalExpr e
evalExpr (Unary '-' e)     = -(evalExpr e)
evalExpr (Binary l '+' r)  = (evalExpr l) + (evalExpr r)
evalExpr (Binary l '-' r)  = (evalExpr l) - (evalExpr r)
evalExpr (Binary l '*' r)  = (evalExpr l) * (evalExpr r)
evalExpr (Binary l '/' r)  = div (evalExpr l) (evalExpr r)
evalExpr (Binary l '%' r)  = rem (evalExpr l) (evalExpr r)
