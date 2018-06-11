module Main where

import GameLogic
import Puzzle

import Test.QuickCheck
import Test.Hspec

word :: String
word = "Asztalitenisz"

puzzle :: Puzzle
puzzle = makePuzzle word

main :: IO ()
main = hspec $ do
  describe "Testing fillInCharacter" $ do
    it "Appending 't' to 'Asztalitenisz' should produce\
           \ _ _ _ t _ _ _ t _ _ _ _ _ Guessed so far: t " $ do
      let
        nils n = take n $ map (const Nothing) [1..5]
        filledInSoFar = nils 3 ++ (Just 't' : nils 3 ++ (Just 't' : nils 5))
        puzzle' = Puzzle word filledInSoFar ['t']
        in fillInCharacter puzzle 't' `shouldBe` puzzle'
          
  describe "Testing handleGuess" $ do
    it "Appending 't' to 'Asztalitenisz' should produce\
           \ _ _ _ t _ _ _ t _ _ _ _ _ Guessed so far: t" $ do
      showPuzzle <- handleGuess puzzle 't'
      let
        str = "_ _ _ t _ _ _ t _ _ _ _ _ Guessed so far: t"
        in show showPuzzle `shouldBe` str
          
