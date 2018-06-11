-- src/Tests.hs

import Test.QuickCheck
import Cipher

strGen :: Gen String
strGen = listOf $ elements ['a'..'z']  

keyPlainTextCaesar :: Gen (Int, String)
keyPlainTextCaesar = do
  k <- arbitrary
  t <- strGen
  return (k, t)

keyPlainTextVigenere :: Gen (String, String)
keyPlainTextVigenere = do
  k <- arbitrary `suchThat` (/= "")
  t <- strGen
  return (k, t)

propCaesarOK :: Property
propCaesarOK =
  forAll keyPlainTextCaesar
  (\(k, t) -> let c = Caesar k
              in ((deCipher c (cipher c t)) == t))

propVigenereOK :: Property
propVigenereOK =
  forAll keyPlainTextVigenere
  (\(k, t) -> let v = Vigenere k
              in ((deCipher v (cipher v t)) == t))

main :: IO ()
main = do
  putStrLn "Testing Caesar cipher implementation..."
  quickCheck propCaesarOK
  putStrLn "Testing Vigenere cipher implementation..."
  quickCheck propVigenereOK
