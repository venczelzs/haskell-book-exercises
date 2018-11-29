-- chapter-exercises.hs

module WordNumberTest where

import WordNumber (digitToWord, digits, wordNumber)
import Test.Hspec
import Test.QuickCheck

import Data.List (sort)
import Data.Char (toUpper)

half x = x / 2
half_id = (*2) . half
prop_half_twice_id = forAll (arbitrary :: Gen Float)
                     (\r -> half_id r == r)

list_ordered :: (Ord a) => [a] -> Bool
list_ordered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

-- list_ordered' :: (Ord a) => [a] -> Bool
-- list_ordered' [] = True
-- list_ordered' (x:xs) = snd $ foldr go (x, True) xs
--   where go _ status@(_, False) = status
--         go x' (x, t)           = (x', x >= x')

prop_list_ordered = forAll (arbitrary :: Gen [Int]) (list_ordered . sort)

-- prop_mine_is_good =
--   let mine = list_ordered' . sort
--       good = list_ordered . sort
--   in forAll (arbitrary :: Gen [Int])
--      (\l -> mine l == good l)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

prop_plusAssociative = forAll (arbitrary :: Gen (Int, Int, Int))
                       (\(x, y, z) -> plusAssociative x y z)
prop_plusCommutative = forAll (arbitrary :: Gen (Int, Int))
                       (\(x, y) -> plusCommutative x y)

multAssociative x y z = x * (y * z) == (x * y) * z
multCommutative x y = x * y == y * x

prop_multAssociative = forAll (arbitrary :: Gen (Int, Int, Int))
                       (\(x, y, z) -> multAssociative x y z)
prop_multCommutative = forAll (arbitrary :: Gen (Int, Int))
                       (\(x, y) -> multCommutative x y)

quotRemOK x y = (quot x y) * y + (rem x y) == x
divModOK x y = (div x y) * y + (mod x y) == x

nonZeroArbitrary :: (Num a, Eq a, Arbitrary a) => Gen a
nonZeroArbitrary = arbitrary `suchThat` (/=0)

rationalArbitrary :: Gen (Integer, Integer)
rationalArbitrary = do
  p <- arbitrary :: Gen Integer
  q <- nonZeroArbitrary
  return (p, q)

prop_quotRemOK = forAll rationalArbitrary (\(p, q) -> quotRemOK p q)
prop_divModOK =  forAll rationalArbitrary (\(p, q) -> divModOK p q)

reverseInvolutive :: Eq a => [a] -> Bool
reverseInvolutive xs = xs == (reverse . reverse $ xs)

prop_reverseInvolutive = forAll (arbitrary :: Gen [String])
                         reverseInvolutive

areTheyEq xs ys = (foldr (:) ys xs) == (xs ++ ys)
areTheyEq2 xs = (foldr (++) [] xs) == (concat xs)

--- this was fun

f n xs = length (take n xs) == n

isThatSo =
  let properValueGenerator :: Gen (Int, [Bool])
      properValueGenerator = do
        n <- arbitrary `suchThat` (>=0)
        bs <- arbitrary `suchThat` (\bs -> n <= length bs)
        return (n, bs)
  in forAll properValueGenerator (\(n, xs) -> f n xs)

---

g a = (read (show a)) == a

testTest1 = forAll (arbitrary :: Gen Float) g
testTest2 = forAll (arbitrary :: Gen String) g
testTest3 = forAll (arbitrary :: Gen Bool) g

---

square x = x * x

squareIdentity = square . sqrt

-- sqrt :: Floating a => a -> a
-- testSquareIdi = forAll (arbitrary :: Gen Int)
--                 (\x -> x == squareIdentity x)

testSquareIdf = forAll (arbitrary :: Gen Float)
                (\x -> x == squareIdentity x)

---

capitalizeWord :: String -> String
capitalizeWord w = case w of
                     ""     -> ""
                     (l:ls) -> toUpper l : ls

twice f = f . f

capitalizeWordIdempotent x =
  capitalizeWord x == twice capitalizeWord x
  
propCapitalizeWordIdempotent = forAll (arbitrary :: Gen String)
                               capitalizeWordIdempotent

sortIdempotent x = sort x == twice sort x
propSortIdempotent = forAll (arbitrary :: Gen [Int]) sortIdempotent

main :: IO ()
main = do
  hspec $ do
    describe "digitToWord does what we want" $ do
      it "returns zero for 0" $ do
        digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ do
        digitToWord 1 `shouldBe` "one"
    
    describe "digits does what we want" $ do
      it "returns [1] for 1" $ do
        digits 1 `shouldBe` [1]
      it "returns [1, 0, 0] for 100" $ do
        digits 100 `shouldBe` [1, 0, 0]
    
    describe "wordNumber does what we want" $ do
      it "returns one-zero-zero for 100" $ do
        wordNumber 100 `shouldBe` "one-zero-zero"
      it "returns nine-zero-zero-one for 9001" $ do
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"

  quickCheck prop_half_twice_id
  quickCheck prop_list_ordered
  -- quickCheck prop_mine_is_good
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative
  quickCheck prop_quotRemOK
  quickCheck prop_divModOK
  quickCheck prop_reverseInvolutive
  quickCheck (forAll (arbitrary :: Gen [Int]) areTheyEq)
  quickCheck (forAll (arbitrary :: Gen [[Int]]) areTheyEq2)
  quickCheck isThatSo
  quickCheck testTest1
  quickCheck testTest2
  quickCheck testTest3
  -- quickCheck testSquareIdi
  -- quickCheck testSquareIdf
  quickCheck propCapitalizeWordIdempotent
  quickCheck propSortIdempotent
