-- src/Puzzle.hs

module Puzzle where

import Data.List (intersperse)
import System.Random (randomRIO)

newtype WordList = WordList [String]
                 deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char]
            deriving Eq

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far with no luck: "
    ++ filter (\ x -> not $ (Just x) `elem` discovered) guessed
    where renderPuzzleChar :: Maybe Char -> Char
          renderPuzzleChar Nothing  = '_'
          renderPuzzleChar (Just c) =  c

makePuzzle :: String -> Puzzle
makePuzzle s = Puzzle s (map (const Nothing) s) []

checkChar :: Puzzle -> Char -> Bool
checkChar (Puzzle s _ _) c = c `elem` s

guessedAlready :: Puzzle -> Char -> Bool
guessedAlready (Puzzle _ _ cs) c = c `elem` cs
               
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) =
  let l = length wl
  in do
    i <- randomRIO (0, l - 1)
    return $ wl !! i

randomWord' :: IO String
randomWord' = gameWords >>= randomWord
