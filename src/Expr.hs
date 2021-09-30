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
   show (FCall n [])       = n ++ "()"
   show (FCall n (x:xs))   = n ++ "(" ++ (show x) ++ (show2 xs) ++ ")"
   show (Paren e)          = "(" ++ (show e) ++ ")"
   show (Unary c e)        = c : (show e)
   show (Binary x c y)     = (show x) ++ [' ', c, ' '] ++ (show y)


