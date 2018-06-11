-- src/Main.hs

module Main where

import qualified Control.Monad      as C.M   (forever, when)
import qualified Data.List          as D.Ls  (intercalate)
import qualified Data.Traversable   as D.Tr  (traverse)
import qualified System.Environment as S.Env (getArgs)
import qualified System.Exit        as S.Ex  (exitFailure, exitSuccess)
import qualified System.IO          as S.IO  (hGetLine, hIsEOF, stdin)

import Morse (stringToMorse, morseToChar)

convertToMorse :: IO ()
convertToMorse = C.M.forever $ do
  weAreDone <- S.IO.hIsEOF S.IO.stdin
  C.M.when weAreDone S.Ex.exitSuccess

  -- otherwise proceed
  line <- S.IO.hGetLine S.IO.stdin
  convertLine line
    where
      convertLine l = do
        let morse = stringToMorse l
        case morse of
          (Just str) -> putStrLn $ D.Ls.intercalate " " str
          Nothing    -> do
            putStrLn $ "ERROR: " ++ l
            S.Ex.exitFailure

convertFromMorse :: IO ()
convertFromMorse = C.M.forever $ do
  weAreDone <- S.IO.hIsEOF S.IO.stdin
  C.M.when weAreDone S.Ex.exitFailure

  -- otherwise proceed
  l <- S.IO.hGetLine S.IO.stdin
  convertLine l
    where
      convertLine l = do
        let decoded :: Maybe String
            decoded = D.Tr.traverse morseToChar (words l)
        case decoded of
          (Just s) -> putStrLn s
          Nothing  -> do
            putStrLn $ "ERROR: " ++ l
            S.Ex.exitFailure

main :: IO ()
main = do
  mode <- S.Env.getArgs
  case mode of
    [arg] -> case arg of
               "from" -> convertFromMorse
               "to"   -> convertToMorse
               _      -> argError
    _     -> S.Ex.exitFailure
  where
    argError = do
      putStrLn "Please specify the first argument \
               \ as being 'from' or 'to' morse,\
               \ such as: morse to"
      S.Ex.exitFailure
