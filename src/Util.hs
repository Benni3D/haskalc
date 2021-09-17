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
