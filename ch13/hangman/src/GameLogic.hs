-- src/GameLogic.hs

module GameLogic where

import Puzzle

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Maybe (isJust)

-- Given a *Char c* and a *Puzzle p*. Returns the *Puzzle p'* which is the product of
--    - the original *String w* word to guess,
--    - a *[Maybe Char] f* list in which characters already filled in are
--      *Just ch* and unknowns are *Nothing*,
--    - and finally a *[Char] g* list that contains all the characters guessed so far.
--
-- If c is in w, then f is appended with Just c, if it is not in w, then it remains
-- unchainged. In either case c is appended to g, unless it was already guessed.
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessedSoFar) guess =
  Puzzle word newFilledInSoFar (guess : guessedSoFar)
  where zipper guess wordChar guessChar =
          if wordChar == guess
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper guess) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (checkChar puzzle guess, guessedAlready puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
               \ but you have forgot..."
      return puzzle
    (True, _) -> do
      putStrLn "Hit!"
      return $ fillInCharacter puzzle guess
    (False, _) -> do
      putStrLn "Nothing"
      return $ fillInCharacter puzzle guess

-- TODO make it better
gameOver :: Puzzle -> IO ()
gameOver (Puzzle word filledInSoFar guessedSoFar) =
  let wrongGuesses =
        length guessedSoFar - (length $ filter isJust filledInSoFar)
  in if wrongGuesses > 10 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ word
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       putStrLn $ "The word was: " ++ word
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is " ++ show puzzle
  putStr "Guess please... "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a\
                    \ single character"
