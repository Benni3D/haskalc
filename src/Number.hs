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

type INumber = Integer
type FNumber = Double

data Number = INum INumber | FNum FNumber

instance Num Number where
   (+) (INum x) (INum y)      = INum $ x + y
   (+) (INum x) (FNum y)      = FNum $ (realToFrac x) + y
   (+) (FNum x) (INum y)      = FNum $ x + (realToFrac y)
   (+) (FNum x) (FNum y)      = FNum $ x + y

   (*) (INum x) (INum y)      = INum $ x * y
   (*) (INum x) (FNum y)      = FNum $ (realToFrac x) * y
   (*) (FNum x) (INum y)      = FNum $ x * (realToFrac y)
   (*) (FNum x) (FNum y)      = FNum $ x * y

   abs (INum x)               = INum $ abs x
   abs (FNum x)               = FNum $ abs x

   signum (INum x)            = INum $ signum x
   signum (FNum x)            = FNum $ signum x

   negate (INum x)            = INum $ -x
   negate (FNum x)            = FNum $ -x

   fromInteger x              = INum x

instance Real Number where
   toRational (INum x)        = toRational x
   toRational (FNum x)        = toRational x

instance Enum Number where
   toEnum x                   = INum $ toInteger x

   fromEnum (INum x)          = fromEnum x
   fromEnum (FNum x)          = fromEnum x

instance Integral Number where
   quotRem (INum x) (INum y)  = (INum a, INum b)
                              where
                                 a = fst $ quotRem x y
                                 b = snd $ quotRem x y
   toInteger (INum x)         = x
   toInteger (FNum x)         = floor x

instance Fractional Number where
   fromRational x             = FNum $ fromRational x
   (/) (INum a) (INum b)      = FNum $ (realToFrac a) / (realToFrac b)
   (/) (INum a) (FNum b)      = FNum $ (realToFrac a) / b
   (/) (FNum a) (INum b)      = FNum $ a / (realToFrac b)
   (/) (FNum a) (FNum b)      = FNum $ a / b

instance RealFrac Number where
   properFraction (FNum x)    = (b, FNum frac)
      where
         (b, frac) = properFraction x
   properFraction (INum x)    = properFraction $ realToFrac x

instance Floating Number where
   pi                         = FNum pi

   exp (FNum x)               = FNum $ exp x
   exp (INum x)               = FNum $ exp $ realToFrac x

   log (FNum x)               = FNum $ log x
   log (INum x)               = FNum $ log $ realToFrac x

   sin (FNum x)               = FNum $ sin x
   sin (INum x)               = FNum $ sin $ realToFrac x

   cos (FNum x)               = FNum $ cos x
   cos (INum x)               = FNum $ cos $ realToFrac x

   asin (FNum x)              = FNum $ asin x
   asin (INum x)              = FNum $ asin $ realToFrac x

   acos (FNum x)              = FNum $ acos x
   acos (INum x)              = FNum $ acos $ realToFrac x

   atan (FNum x)              = FNum $ atan x
   atan (INum x)              = FNum $ atan $ realToFrac x

   sinh (FNum x)              = FNum $ sinh x
   sinh (INum x)              = FNum $ sinh $ realToFrac x

   cosh (FNum x)              = FNum $ cosh x
   cosh (INum x)              = FNum $ cosh $ realToFrac x

   asinh (FNum x)             = FNum $ asinh x
   asinh (INum x)             = FNum $ asinh $ realToFrac x

   acosh (FNum x)             = FNum $ acosh x
   acosh (INum x)             = FNum $ acosh $ realToFrac x

   atanh (FNum x)             = FNum $ atanh x
   atanh (INum x)             = FNum $ atanh $ realToFrac x

   (**) (INum x) (INum y)     | y >= 0    = INum $ x ^ y
                              | otherwise = FNum $ (realToFrac x) ** (realToFrac y)

   (**) (INum x) (FNum y)                 = FNum $ (realToFrac x) ** y
   (**) (FNum x) (INum y)                 = FNum $ x ** (realToFrac y)
   (**) (FNum x) (FNum y)                 = FNum $ x ** y


instance Eq Number where
   (==) (INum x) (INum y)     = x == y
   (==) (INum x) (FNum y)     = (realToFrac x) == y
   (==) (FNum x) (INum y)     = x == (realToFrac y)
   (==) (FNum x) (FNum y)     = x == y

instance Ord Number where
   (<=) (INum x) (INum y)     = x <= y
   (<=) (INum x) (FNum y)     = (realToFrac x) <= y
   (<=) (FNum x) (INum y)     = x <= (realToFrac y)
   (<=) (FNum x) (FNum y)     = x <= y
   

instance Show Number where
   show (INum x)              = show x
   show (FNum x)              = show x

pow10 :: Integer -> Number
pow10 e  | e >= 0    = INum $ 10 ^ e
         | otherwise = FNum $ 10.0 ** (realToFrac e)
