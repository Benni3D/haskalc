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

module Context where
import Control.Monad.State
import Number
import Expr

type Variable = (String, Number)
type Function = (String, [String], Expr)
type SubEvalEnv = ([Variable], [Function])
type EvalEnv = ([Variable], [Function], [SubEvalEnv])
type EvalContext = State EvalEnv

envAddVar :: Variable -> EvalContext Number
envAddVar v = do
   (vars, funcs, stack) <- get
   put $ (internal_addVar v vars, funcs, stack)
   return (snd v)

envAddVars :: [Variable] -> EvalContext ()
envAddVars []     = return ()
envAddVars (x:xs) = do
   envAddVar x
   envAddVars xs
   return ()

envGetVar :: String -> EvalContext (Maybe Number)
envGetVar name = do
   (vars, _, _) <- get
   return $ internal_findVar name vars

envAddFunc :: Function -> EvalContext ()
envAddFunc f = do
   (vars, funcs, stack) <- get
   put $ (vars, internal_addFunc f funcs, stack)
   return ()

envGetFunc :: String -> EvalContext (Maybe Function)
envGetFunc name = do
   (_, funcs, _) <- get
   return $ internal_findFunc name funcs

envPushContext :: EvalContext ()
envPushContext = do
   (vars, funcs, stack) <- get
   put (vars, funcs, (vars, funcs) : stack)
   return ()

envPopContext :: EvalContext ()
envPopContext = do
   env <- get
   put $ internal_popEnv env
   return ()

emptyEvalEnv :: EvalEnv
emptyEvalEnv = ([], [], [])


showFuncDecl :: Function -> String
showFuncDecl (name, [], _)    = name ++ "()"
showFuncDecl (name, (x:xs), _)= name ++ "(" ++ x ++ (internal_showFuncParams xs) ++ ")"

-- INTERNAL FUNCTIONS

internal_popEnv :: EvalEnv -> EvalEnv
internal_popEnv (_, _, [])                      = error "stack is empty"
internal_popEnv (_, _, (vars, funcs) : stack)   = (vars, funcs, stack)

internal_showFuncParams :: [String] -> String
internal_showFuncParams []       = ""
internal_showFuncParams (x:xs)   = (", " ++ x) ++ (internal_showFuncParams xs)


internal_findVar :: String -> [Variable] -> Maybe Number
internal_findVar name []                     = Nothing
internal_findVar name ((n,v):xs) | n == name = Just v
                                 | otherwise = internal_findVar name xs

internal_addVar :: Variable -> [Variable] -> [Variable]
internal_addVar v []                   = [v]
internal_addVar (n, v) ((en, ev):xs)   | n == en   = (n, v) : xs
                                       | otherwise = (en, ev) : internal_addVar (n, v) xs

internal_findFunc :: String -> [Function] -> Maybe Function
internal_findFunc name []              = Nothing
internal_findFunc name ((n,p,e):xs)    | name == n       = Just (n,p,e)
                                       | otherwise       = internal_findFunc name xs

internal_addFunc :: Function -> [Function] -> [Function]
internal_addFunc f []                     = [f]
internal_addFunc (n,p,e) ((nx,px,ex):xs)  | n == nx      = (n,p,e) : xs
                                          | otherwise    = (nx,px,ex) : internal_addFunc (n,p,e) xs
