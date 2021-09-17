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

{-# LANGUAGE CPP #-}
#if ENABLE_READLINE
import System.Console.Readline
#endif

import System.Exit
import System.IO
import Parsing
import Eval

run :: Expr -> [Char]
run (Error e)        = "Error: " ++ e
run e                = show $ evalExpr e

main :: IO()
#if ENABLE_READLINE
main =   do
         maybeLine <- readline "% "
         case maybeLine of
            Nothing     -> exitSuccess
            Just "exit" -> exitSuccess
            Just []     -> main
            Just line   -> putStrLn $ run $ parse_expr line
         main
#else
main =   do
         putStr "% "
         hFlush stdout
         line_in   <- getLine
         case line_in of
            ""       -> main
            "exit"   -> exitSuccess
            line     -> putStrLn $ run $ parse_expr line
         main

#endif
