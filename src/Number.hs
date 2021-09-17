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

module Number where

data Number = IVal Integer | FVal Float

instance Num Number where
   (+) (IVal x) (IVal y)      = IVal $ x + y
   (+) (IVal x) (FVal y)      = FVal $ (realToFrac x) + y
   (+) (FVal x) (IVal y)      = FVal $ x + (realToFrac y)
   (+) (FVal x) (FVal y)      = FVal $ x + y

   (*) (IVal x) (IVal y)      = IVal $ x * y
   (*) (IVal x) (FVal y)      = FVal $ (realToFrac x) * y
   (*) (FVal x) (IVal y)      = FVal $ x * (realToFrac y)
   (*) (FVal x) (FVal y)      = FVal $ x * y

   abs (IVal x)               = IVal $ abs x
   abs (FVal x)               = FVal $ abs x

   signum (IVal x)            = IVal $ signum x
   signum (FVal x)            = FVal $ signum x

   negate (IVal x)            | x < 0     = IVal $ -x
                              | otherwise = IVal $  x

   negate (FVal x)            | x < 0     = FVal $ -x
                              | otherwise = FVal $  x

   fromInteger x              = IVal x

instance Real Number where
   toRational (IVal x)        = toRational x
   toRational (FVal x)        = toRational x

instance Enum Number where
   toEnum x                   = IVal $ toInteger x

   fromEnum (IVal x)          = fromEnum x
   fromEnum (FVal x)          = fromEnum x

instance Integral Number where
   quotRem (IVal x) (IVal y)  = (IVal a, IVal b)
                              where
                                 a = fst $ quotRem x y
                                 b = snd $ quotRem x y
   toInteger (IVal x)         = x
   toInteger (FVal x)         = floor x

instance Fractional Number where
   fromRational x             = FVal $ fromRational x
   (/) (IVal a) (IVal b)      = FVal $ (realToFrac a) / (realToFrac b)
   (/) (IVal a) (FVal b)      = FVal $ (realToFrac a) / b
   (/) (FVal a) (IVal b)      = FVal $ a / (realToFrac b)
   (/) (FVal a) (FVal b)      = FVal $ a / b

instance Eq Number where
   (==) (IVal x) (IVal y)     = x == y
   (==) (IVal x) (FVal y)     = (realToFrac x) == y
   (==) (FVal x) (IVal y)     = x == (realToFrac y)
   (==) (FVal x) (FVal y)     = x == y

instance Ord Number where
   (<=) (IVal x) (IVal y)     = x <= y
   (<=) (IVal x) (FVal y)     = (realToFrac x) <= y
   (<=) (FVal x) (IVal y)     = x <= (realToFrac y)
   (<=) (FVal x) (FVal y)     = x <= y
   

instance Show Number where
   show (IVal x)              = show x
   show (FVal x)              = show x

