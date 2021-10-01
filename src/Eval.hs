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

module Eval where
import Control.Monad.State
import Context
import Number
import Value
import Expr

type EvalResult = Either [Char] Value

showEvalResult :: EvalResult -> [Char]
showEvalResult (Right r)      = show r
showEvalResult (Left msg)     = "Error: " ++ msg

evalNumber :: Expr -> EvalContext (Either String Number)
evalNumber e   = do
   res <- evalExpr e
   case res of
      Left msg -> return $ Left msg
      Right x  ->
         case x of
            (NVal n) -> return $ Right n
            _        -> return $ Left $ "`" ++ (show e) ++ "` is not a number."

evalNumbers :: (Expr, Expr) -> EvalContext (Either String (Number, Number))
evalNumbers (mx, my) = do
   res1 <- evalNumber mx
   res2 <- evalNumber my
   case res1 of
      Left msg -> return $ Left msg
      Right x  ->
         case res2 of
            Left msg -> return $ Left msg
            Right y  -> return $ Right (x, y)

do_binary :: (Number -> Number -> Number) -> Expr -> Expr -> EvalContext EvalResult
do_binary f l r = do
   res <- evalNumbers (l, r)
   case res of
      Left s         -> return $ Left s
      Right (x, y)   -> return $ Right $ NVal $ f x y

do_compare :: (Number -> Number -> Bool) -> Expr -> Expr -> EvalContext EvalResult
do_compare f l r = do
   res <- evalNumbers (l, r)
   case res of
      Left s         -> return $ Left s
      Right (x, y)   -> return $ Right $ BVal $ f x y

do_fcall1 :: String -> (Number -> Number) -> [Expr] -> EvalContext EvalResult
do_fcall1 _ f [e] = do
   res <- evalNumber e
   case res of
      Left msg -> return $ Left msg
      Right n  -> return $ Right $ NVal $ f n
do_fcall1 n _ _   = return $ Left $ n ++ "(x): invalid argument count"

do_fcall2 :: String -> (Number -> Number -> Number) -> [Expr] -> EvalContext EvalResult
do_fcall2 _ f [l, r] = do
   res <- evalNumbers (l, r)
   case res of
      Left msg    -> return $ Left msg
      Right (x,y) -> return $ Right $ NVal $ f x y
do_fcall2 n _ _      = return $ Left $ n ++ "(x, y): invalid argument count"


evalExpr :: Expr -> EvalContext EvalResult
evalExpr (Error msg)          = return $ Left msg
evalExpr (Val n)              = return $ Right $ NVal n
evalExpr (Paren e)            = evalExpr e
evalExpr (Unary '+' e)        = evalExpr e
evalExpr (Unary '-' e)        = do
   res <- evalNumber e
   case res of
      Left msg -> return $ Left msg
      Right x  -> return $ Right $ NVal $ -x
evalExpr (Binary l '+' r)     = do_binary (+) l r
evalExpr (Binary l '-' r)     = do_binary (-) l r
evalExpr (Binary l '*' r)     = do_binary (*) l r
evalExpr (Binary l '/' r)     = do_binary (/) l r
evalExpr (Binary l '%' r)     = do_binary rem l r
evalExpr (Binary l '^' r)     = do_binary (**) l r
evalExpr (Binary l '<' r)     = do_compare (<) l r
evalExpr (Binary l '>' r)     = do_compare (>) l r
evalExpr (Cond c l r)         = do
   tmp <- evalExpr c
   case tmp of
      Left msg    -> return $ Left msg
      Right cond  ->
         case cond of
            (BVal b) ->
               case b of
                  True  -> evalExpr l
                  False -> evalExpr r
            _        -> return $ Left $ "`" ++ (show c) ++ "` is not a boolean value."

-- Assignment to Variable

evalExpr (Binary (Var name) '=' r) = do
   mv <- evalExpr r
   case mv of
      (Left msg)  -> return $ Left msg
      (Right v)   -> do
         envAddVar (name,v)
         return $ Right NoVal

-- Definition of Function

evalExpr (Binary (FCall name args) '=' r) = do
   case el_to_pl args of
      Nothing  -> return $ Left $ "failed to parse argument list of " ++ name ++ "."
      Just a   -> do
         envAddFunc (name, a, r)
         return $ Right NoVal

-- Assignment to invalid

evalExpr (Binary l '=' _)     = return $ Left $ "cannot assign to `" ++ (show l) ++ "`"

-- Built-in functions

evalExpr (FCall "sin"      a) = do_fcall1 "sin"       sin            a
evalExpr (FCall "cos"      a) = do_fcall1 "cos"       cos            a
evalExpr (FCall "tan"      a) = do_fcall1 "tan"       tan            a
evalExpr (FCall "sinh"     a) = do_fcall1 "sinh"      sinh           a
evalExpr (FCall "cosh"     a) = do_fcall1 "cosh"      cosh           a
evalExpr (FCall "tanh"     a) = do_fcall1 "tanh"      tanh           a
evalExpr (FCall "asin"     a) = do_fcall1 "asin"      asin           a
evalExpr (FCall "acos"     a) = do_fcall1 "acos"      acos           a
evalExpr (FCall "atan"     a) = do_fcall1 "atan"      atan           a
evalExpr (FCall "asinh"    a) = do_fcall1 "asinh"     asinh          a
evalExpr (FCall "acosh"    a) = do_fcall1 "acosh"     acosh          a
evalExpr (FCall "atanh"    a) = do_fcall1 "atanh"     atanh          a
evalExpr (FCall "sqrt"     a) = do_fcall1 "sqrt"      sqrt           a
evalExpr (FCall "log"      a) = do_fcall1 "log"       log            a
evalExpr (FCall "log2"     a) = do_fcall1 "log2"      (logBase 2)    a
evalExpr (FCall "log10"    a) = do_fcall1 "log10"     (logBase 10)   a
evalExpr (FCall "ceil"     a) = do_fcall1 "ceil"      ceiling        a
evalExpr (FCall "floor"    a) = do_fcall1 "floor"     floor          a
evalExpr (FCall "truncate" a) = do_fcall1 "truncate"  truncate       a
evalExpr (FCall "round"    a) = do_fcall1 "round"     round          a
evalExpr (FCall "gcd"      a) = do_fcall2 "gcd"       gcd            a
evalExpr (FCall "lcm"      a) = do_fcall2 "lcm"       lcm            a

-- Built-in variables
evalExpr (Var "pi")           = return $ Right $ NVal $ FNum pi
evalExpr (Var "true")         = return $ Right $ BVal True
evalExpr (Var "false")        = return $ Right $ BVal False

-- Function Call to user-defined function

evalExpr (FCall name a)       = do
   mf <- envGetFunc name
   envPushContext
   case mf of
      Nothing  -> return $ Left $ "No such function " ++ name ++ " exists."
      Just f   -> evalFunc f a

-- User-defined variables

evalExpr (Var name)           = do
   mv <- envGetVar name
   case mv of
      Nothing  -> return $ Left $ "Variable " ++ name ++ " does not exist."
      Just v   -> return $ Right v

-- Invalid/Unsupported expression

evalExpr e                    = return $ Left $ "failed to evaluate `" ++ (show e) ++ "`"

-- Helper functions

evalParams :: [(String, Expr)] -> EvalContext (Either String [(String, Value)])
evalParams []           = return $ Right []
evalParams ((n,e):xs)   = do
   mv <- evalExpr e
   case mv of
      Left msg -> return $ Left msg
      Right v  -> evalParams xs >>= (\mr -> return $ mr >>= (\r -> Right $ (n, v) : r))

evalFunc :: Function -> [Expr] -> EvalContext EvalResult
evalFunc (n,p,e) args   | (length args) /= (length p) =
                           return $ Left $ (showFuncDecl (n,p,e)) ++ ": invalid argument count."
                        | otherwise = do
   err <- evalParams $ zip p args
   case err of
      Left msg -> return $ Left msg
      Right p  -> do
         envAddVars p
         val <- evalExpr e
         envPopContext
         return val
