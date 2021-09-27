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

import Control.Monad.State
import System.Exit
import System.IO
import Context
import Parsing
import Eval
import Expr

do_eval :: Expr -> EvalContext (EvalEnv, EvalResult)
do_eval e = do
   v <- evalExpr e
   env <- get
   return (env, v)
   

run :: EvalEnv -> Expr -> (EvalEnv, String)
run env e =
   case res of
   Left msg    -> (new_env, "Error: " ++ msg)
   Right val   -> (new_env, show val)
   where
      (new_env, res) = evalState (do_eval e) env

loop :: EvalEnv -> IO ()

#if ENABLE_READLINE
loop env = do
   maybeLine <- readline "% "
   case maybeLine of
      Nothing     -> exitSuccess
      Just "exit" -> exitSuccess
      Just []     -> loop env
      Just line   -> do
         expr <- return $ parse_expr line
         (new_env, str) <- return $ run env expr
         putStrLn str
         loop new_env
#else
loop env = do
   putStr "% "
   hFlush stdout
   line_in   <- getLine
   case line_in of
      ""       -> loop env
      "exit"   -> exitSuccess
      line     -> do
         expr <- return $ parse_expr line
         (new_env, str) <- return $ run env expr
         putStrLn str
         loop new_env
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
   loop []


