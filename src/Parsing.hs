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
import Util

data Expr = Val Int | Paren Expr | Unary Char Expr | Binary Expr Char Expr;

showExpr :: Expr -> [Char]
showExpr (Val n)        = show n
showExpr (Paren e)      = "(" ++ (showExpr e) ++ ")"
showExpr (Unary c e)    = c : (showExpr e)
showExpr (Binary x c y) = (showExpr x) ++ [' ', c, ' '] ++ (showExpr y)

do_parse_int :: [Char] -> (Maybe Int, [Char])
do_parse_int []                     =  (Nothing, [])
do_parse_int (x:xs)  | xs == []     =  (from_digit x, [])
                     | otherwise    =  case from_digit x of
                                       Nothing  -> (Nothing, (x:xs))
                                       Just d   -> case do_parse_int xs of
                                                   (Nothing, r)   -> (Just d, xs)
                                                   (Just n, r)    -> (Just $ n + (d * 10^(num_digits n)), r)
parse_int :: [Char] -> (Maybe Expr, [Char])
parse_int s =  case do_parse_int $ skip_ws s of
               (Nothing, r)   -> (Nothing, r)
               (Just n, r)    -> (Just $ Val n, r)


do_parse_expr :: [Char] -> (Maybe Expr, [Char])

match_paren :: (Maybe Expr, [Char]) -> (Maybe Expr, [Char])
match_paren (Nothing, r)   = (Nothing, r)
match_paren (e, r:rs)      | isSpace r = match_paren (e, rs)
                           | r == ')'  =  case e of
                                          Nothing  -> (Nothing, r:rs)
                                          Just e   -> (Just $ Paren e, rs)
                           | otherwise = (Nothing, r:rs)

parse_paren :: [Char] -> (Maybe Expr, [Char])
parse_paren (x:xs)   | isSpace x    = parse_paren xs
                     | x == '('     = match_paren $ do_parse_expr xs
                     | otherwise    = parse_int $ x:xs

parse_unary :: [Char] -> (Maybe Expr, [Char])
parse_unary (x:xs)      | isSpace x             = parse_unary xs
                        | x == '+' || x == '-'  =  case parse_unary xs of
                                                   (Nothing, r) -> (Nothing, r)
                                                   (Just e, r)  -> (Just $ Unary x e, r)
                        | otherwise             =  parse_paren $ x:xs

get_op :: [Char] -> [Char] -> (Maybe Char, [Char])
get_op [] ops     = (Nothing, [])
get_op (x:xs) ops | isSpace x    = get_op xs ops
                  | elem x ops   = (Just x, skip_ws xs)
                  | otherwise    = (Nothing, x:xs)

parse_binary :: [Char] -> [Char] -> ([Char] -> (Maybe Expr, [Char])) -> (Maybe Expr, [Char])
parse_binary str ops f  =  case f str of
                           (Nothing, rl)  -> (Nothing, rl)
                           (Just left, rl)-> case get_op rl ops of
                                             (Nothing, rop) -> (Just left, rl)
                                             (Just op, rop) -> case parse_binary rop ops f of
                                                         (Nothing, rr)     -> (Nothing, rr)
                                                         (Just right, rr)  -> (Just $ Binary left op right, rr)
                        
parse_muldiv str = parse_binary str "*/%" parse_unary
parse_addsub str = parse_binary str "+-" parse_muldiv

do_parse_expr str = parse_addsub str

parse_expr :: [Char] -> Maybe Expr
parse_expr str =  case do_parse_expr str of
                  (Nothing, r)   -> Nothing
                  (Just e, r)    -> if r == [] then Just e else Nothing
