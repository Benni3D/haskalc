{-# LANGUAGE CPP #-}
#if ENABLE_READLINE
import System.Console.Readline
#endif

import System.Exit
import System.IO
import Parsing
import Eval

run :: Maybe Expr -> [Char]
run Nothing          = "Error"
run (Just e)         = show $ evalExpr e

main :: IO()
#if ENABLE_READLINE
main =   do
         maybeLine <- readline "% "
         case maybeLine of
            Nothing     -> exitSuccess
            Just "exit" -> exitSuccess
            Just line   -> putStrLn $ run $ parse_expr line
         main
#else
main =   do
         putStr "> "
         hFlush stdout
         line   <- getLine
         if line == [] then putStr "" else do
            putStrLn $ run $ parse_expr line
            main
#endif
