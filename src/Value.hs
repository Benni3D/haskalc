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

module Value where
import Number

data Value = NVal Number | BVal Bool | NoVal

instance Eq Value where
   (==) (NVal x) (NVal y)  = x == y
   (==) (BVal x) (BVal y)  = x == y
   (==) (NoVal)  (NoVal)   = True
   (==) _ _                = False

instance Show Value where
   show (NVal x)  = show x
   show (BVal x)  = show x
   show (NoVal)   = ""

valGetNum :: Value -> Maybe Number
valGetNum (NVal x)   = Just x
valGetNum _          = Nothing

valGetBool :: Value -> Maybe Bool
valGetBool (BVal x)  = Just x
valGetBool _         = Nothing
