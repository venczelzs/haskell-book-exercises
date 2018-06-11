-- cipher.hs

module Cipher (
  Cipher (cipher, deCipher),
  Caesar (Caesar),
  Vigenere (Vigenere)
  ) where

import Data.Char

class Cipher c where
  cipher :: c -> String -> String
  deCipher :: c -> String -> String

data Caesar = Caesar Int

instance Cipher Caesar where
  cipher (Caesar k) s = caesar s k
  deCipher (Caesar k) s = unCaesar s k 

data Vigenere = Vigenere String

instance Cipher Vigenere where
  cipher (Vigenere k) s = vigenere s k
  deCipher (Vigenere k) s = unVigenere s k

alphabetStart = 97 :: Int
alphabetLength = 26 :: Int

caesar :: String -> Int -> String
caesar "" _ = ""
caesar (c:cs) k = shift c k : caesar cs k

shift :: Char -> Int -> Char
shift c k
  | c `elem` ":;<=>?@[\\]^_` " = c
  | otherwise = if isUpper c then
                  toUpper $ shift (toLower c) k
                else
                  chr $ (k + ord c - alphabetStart) `mod` alphabetLength + alphabetStart
  
unCaesar :: String -> Int -> String
unCaesar s k = caesar s (-k)

type Keyword = String

vigenere :: String -> Keyword -> String
vigenere s k = go s (cycle k)
  where
    go :: [Char] -> [Char] -> [Char]
    go [] _ = []
    go (c:cs) cs' =
      case c of
        ' ' -> ' ' : go cs cs'
        _   -> (shift c k') : go cs (tail cs')
          where
            k' = ord (toLower $ head cs') - alphabetStart
  
unVigenere :: String -> Keyword -> String
unVigenere s k = vigenere s (mirror k)

mirror :: Keyword -> Keyword
mirror = foldr (\c cs -> mirror' c : cs) []

mirror' :: Char -> Char
mirror' c =
  case isUpper c of
    True  -> toUpper $ chr ((alphabetStart - (ord $ toLower c)) `mod` alphabetLength + alphabetStart)
    False -> chr ((alphabetStart - ord c) `mod` alphabetLength + alphabetStart)

-- key = "ALLY"
-- plainText = "MEET AT DAWN"
-- cipherText = vigenere plainText key

-- main :: IO ()
-- main = do
--   if unVigenere cipherText key == plainText
--     then
--     putStrLn "OK"
--     else
--     putStrLn "NOT OK"
