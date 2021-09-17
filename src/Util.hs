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

module Util where
import Data.Char

find :: (Eq a) => a -> [a] -> Maybe Int
find e []         = Nothing
find e (x:xs)     | e == x    = Just 0
                  | otherwise = do  n <- find e xs
                                    Just $ 1 + n

num_digits :: Int -> Int
num_digits 0 = 1
num_digits n = ceiling $ logBase 10 $ realToFrac $ n + 1

from_digit :: Char -> Maybe Int
from_digit ch  | isDigit ch   = Just $ digitToInt ch
               | otherwise    = Nothing

skip_ws :: [Char] -> [Char]
skip_ws []     = []
skip_ws (x:xs) | isSpace x = skip_ws xs
               | otherwise = x:xs
