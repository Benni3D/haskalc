module Expr where
import Control.Monad
import Number


data Expr   = Val Number
            | Var [Char]
            | FCall [Char] [Expr]
            | Paren Expr
            | Unary Char Expr
            | Binary Expr Char Expr
            | Error [Char]

show2 :: [Expr] -> [Char]
show2 []       = ""
show2 (x:xs)   = "," ++ (show x) ++ (show2 xs)

instance Show Expr where
   show (Val n)            = show n
   show (Var n)            = n
   show (FCall n [])       = (show n) ++ "()"
   show (FCall n (x:xs))   = (show n) ++ "(" ++ (show x) ++ (show2 xs) ++ ")"
   show (Paren e)          = "(" ++ (show e) ++ ")"
   show (Unary c e)        = c : (show e)
   show (Binary x c y)     = (show x) ++ [' ', c, ' '] ++ (show y)


