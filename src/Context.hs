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

type Variable = (String, Number)
type EvalEnv = [Variable]
type EvalContext = State EvalEnv

envAddVar :: Variable -> EvalContext Number
envAddVar v = do
   env <- get
   put $ internal_addVar v env
   return (snd v)

envGetVar :: String -> EvalContext (Maybe Number)
envGetVar name = do
   env <- get
   return $ internal_findVar name env





internal_findVar :: String -> EvalEnv -> Maybe Number
internal_findVar name []                     = Nothing
internal_findVar name ((n,v):xs) | n == name = Just v
                                 | otherwise = internal_findVar name xs

internal_addVar :: Variable -> EvalEnv -> EvalEnv
internal_addVar v []                   = [v]
internal_addVar (n, v) ((en, ev):xs)   | n == en   = (n, v) : xs
                                       | otherwise = internal_addVar (n, v) xs
