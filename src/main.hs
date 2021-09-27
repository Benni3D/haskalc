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

#if ENABLE_POSIX
import System.Posix.Signals
#endif

import System.Exit
import System.IO
import Parsing
import Eval
import Expr

run :: Expr -> [Char]
run (Error e)        = "Error: " ++ e
run e                = show $ evalExpr e

loop :: IO ()

#if ENABLE_READLINE
loop = do
   maybeLine <- readline "% "
   case maybeLine of
      Nothing     -> exitSuccess
      Just "exit" -> exitSuccess
      Just []     -> loop
      Just line   -> putStrLn $ run $ parse_expr line
   loop
#else
loop = do
   putStr "% "
   hFlush stdout
   line_in   <- getLine
   case line_in of
      ""       -> loop
      "exit"   -> exitSuccess
      line     -> putStrLn $ run $ parse_expr line
   loop
#endif

#if ENABLE_POSIX
handle_sigINT :: IO ()
handle_sigINT = do
   hFlush stdout
   putStrLn "Int"
   hFlush stdout
#endif

main :: IO ()
main = do
#if ENABLE_POSIX
   installHandler sigINT (Catch (handle_sigINT)) (Just emptySignalSet)
   putStrLn "registered"
#endif
   loop


