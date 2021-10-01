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
            | Var String
            | FCall String [Expr]
            | Paren Expr
            | Unary Char Expr
            | Binary Expr Char Expr
            | Cond Expr Expr Expr
            | Error String

show2 :: [Expr] -> [Char]
show2 []       = ""
show2 (x:xs)   = "," ++ (show x) ++ (show2 xs)

internal_e2p :: Expr -> Maybe String
internal_e2p (Var name) = Just name
internal_e2p _          = Nothing

-- expr list to parameter list
el_to_pl :: [Expr] -> Maybe [String]
el_to_pl []       = Just []
el_to_pl (x:xs)   = do
   l <- internal_e2p x
   r <- el_to_pl xs
   return $ l : r

instance Show Expr where
   show (Val n)            = show n
   show (Var n)            = n
   show (FCall n [])       = n ++ "()"
   show (FCall n (x:xs))   = n ++ "(" ++ (show x) ++ (show2 xs) ++ ")"
   show (Paren e)          = "(" ++ (show e) ++ ")"
   show (Unary c e)        = c : (show e)
   show (Binary x c y)     = (show x) ++ [' ', c, ' '] ++ (show y)


