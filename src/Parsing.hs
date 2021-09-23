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

module Parsing where
import Data.Char
import Number
import Util

data Expr = Val Number | Paren Expr | Unary Char Expr | Binary Expr Char Expr | Error [Char]

showExpr :: Expr -> [Char]
showExpr (Val n)        = show n
showExpr (Paren e)      = "(" ++ (showExpr e) ++ ")"
showExpr (Unary c e)    = c : (showExpr e)
showExpr (Binary x c y) = (showExpr x) ++ [' ', c, ' '] ++ (showExpr y)

-- TODO: Extend to parsing decimals
do_parse_int :: [Char] -> (Maybe INumber, [Char])
do_parse_int []                     =  (Nothing, [])
do_parse_int (x:xs)  | xs == []     =  (from_digit x, [])
                     | otherwise    =  case from_digit x of
                                       Nothing  -> (Nothing, (x:xs))
                                       Just d   -> case do_parse_int xs of
                                                   (Nothing, r)   -> (Just d, xs)
                                                   (Just n, r)    -> (Just $ n + (d * 10^(num_digits n)), r)

do_parse_float :: [Char] -> (Maybe Number, [Char])
do_parse_float s =
   case do_parse_int s of
   (Nothing, r)   -> (Nothing, r)
   (Just n,  r)   ->
      case r of
      ('.':rs) ->
         case do_decimal (-1) rs of
         (Nothing, r2)  -> (Nothing, r2)
         (Just fp, r2)  -> (Just $ FVal $ (realToFrac n) + fp, r2)
      rs       -> (Just $ IVal n, r)
                     

do_decimal :: Int -> [Char] -> (Maybe FNumber, [Char])
do_decimal _ []      =  (Nothing, [])
do_decimal e (x:xs)  =  case from_digit x of
                        Nothing  -> (Nothing, (x:xs))
                        Just d   -> case do_decimal (e - 1) xs of
                                    (Nothing, r)   -> (Just $ (realToFrac d) * z, r)
                                    (Just n,  r)   -> (Just $ n + ((realToFrac d) * z), r)
                        where
                           z = 10.0 ** (realToFrac e) :: FNumber
                           
do_unary_int :: [Char] -> (Maybe Integer, [Char])
do_unary_int ('+':xs)   = do_unary_int xs
do_unary_int ('-':xs)   =
   case do_unary_int xs of
   (Nothing, r)   -> (Nothing, r)
   (Just x,  r)   -> (Just $ -x, r)
do_unary_int s          = do_parse_int s

parse_float :: [Char] -> (Expr, [Char])
parse_float s =
   case do_parse_float s of
   (Nothing, r)   -> (Error "failed to parse number", r)
   (Just x, r)    ->
      case r of
      ('e':rs) ->
         case do_unary_int rs of
         (Nothing, r2)  -> (Error "expected exponent", r2)
         (Just e,  r2)  -> (Val $ x * (pow10 e), r2)
      rs       -> (Val x, r)
   

parse_num :: [Char] -> (Expr, [Char])
parse_num s = parse_float s


do_parse_expr :: [Char] -> (Expr, [Char])

match_paren :: (Expr, [Char]) -> (Expr, [Char])
match_paren (Error e, r)   = (Error e, r)
match_paren (e, [])        = (Error "missing ')'", [])
match_paren (e, r:rs)      | isSpace r = match_paren (e, rs)
                           | r == ')'  = (Paren e, rs)
                           | otherwise = (Error "missing ')'", r:rs)

parse_paren :: [Char] -> (Expr, [Char])
parse_paren (x:xs)   | isSpace x    = parse_paren xs
                     | x == '('     = match_paren $ do_parse_expr xs
                     | otherwise    = parse_num $ x:xs

parse_unary :: [Char] -> (Expr, [Char])
parse_unary []          = (Error "unexpected end of line", [])
parse_unary (x:xs)      | isSpace x             = parse_unary xs
                        | x == '+' || x == '-'  =  case parse_unary xs of
                                                   (Error e, r) -> (Error e, r)
                                                   (e, r)  -> (Unary x e, r)
                        | otherwise             =  parse_paren $ x:xs

get_op :: [Char] -> [Char] -> (Maybe Char, [Char])
get_op [] ops     = (Nothing, [])
get_op (x:xs) ops | isSpace x    = get_op xs ops
                  | elem x ops   = (Just x, skip_ws xs)
                  | otherwise    = (Nothing, x:xs)

parse_binary :: [Char] -> [Char] -> ([Char] -> (Expr, [Char])) -> (Expr, [Char])
parse_binary str ops f  =  case f str of
                           (Error e, rl)  -> (Error e, rl)
                           (left, rl)     -> case get_op rl ops of
                                             (Nothing, rop) -> (left, rl)
                                             (Just op, rop) -> case parse_binary rop ops f of
                                                         (Error e, rr)     -> (Error e, rr)
                                                         (right, rr)       -> (Binary left op right, rr)
                        
parse_muldiv str = parse_binary str "*/%" parse_unary
parse_addsub str = parse_binary str "+-" parse_muldiv

do_parse_expr str = parse_addsub str

parse_expr :: [Char] -> Expr
parse_expr str =  case do_parse_expr str of
                  (Error e, r)   -> Error e
                  (e, r)         -> if (skip_ws r) == [] then e else Error "expected end of line, got garbage"
