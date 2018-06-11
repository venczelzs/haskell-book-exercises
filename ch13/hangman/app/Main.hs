module Main where

import GameLogic
import Puzzle

import System.IO (hSetBuffering, stdout, BufferMode( NoBuffering ))
import Data.Char (toLower)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = makePuzzle (fmap toLower word)
  runGame puzzle
