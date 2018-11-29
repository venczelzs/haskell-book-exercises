-- chapterExercises.hs

module WordNumber where

import Data.List (intersperse)

sumOneToN :: (Eq a, Num a) => a -> a
sumOneToN n = go 0 n
  where go acc n
          | n == 0    = acc
          | otherwise = go (acc + n) (n - 1)

-- sumOneToN :: Fractional a => a -> a
-- sumOneToN n = n * (n + 1) / 2

-- mult :: Integral a => a -> a -> a
-- mult _ 0 = 0
-- mult 0 _ = 0
-- mult x 1 = x
-- mult 1 y = y
-- mult x y
--   | y > 0     = x + mult x (y - 1)
--   | otherwise = (-x) + mult (-x) (y - 1)

mc91 :: Integral a => a -> a
mc91 n
  | n > 100  = n - 10
  | otherwise = mc91 $ mc91 $ n + 11

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n

digits :: Int -> [Int]
digits n = go n q
  where
    q = floor((log (fromIntegral n)) / (log 10))
    go :: Int -> Int -> [Int]
    go n q
      | q == 0    = n : []
      | otherwise = div n (10^q) : go (mod n (10^q)) (q - 1)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "error"
