import System.IO
import Parsing
import Eval

run :: Maybe Expr -> [Char]
run Nothing          = "Error"
run (Just e)         = show $ evalExpr e

main :: IO()
main =   do
         putStr "> "
         hFlush stdout
         str   <- getLine
         if str == [] then putStr "" else do
            putStrLn $ run $ parse_expr str
            main
         
