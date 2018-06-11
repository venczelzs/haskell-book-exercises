module Main where

import qualified Cipher as C

import System.IO

main :: IO ()
main =
  let c = C.Caesar 17
      v = C.Vigenere "teniszütő"
  in do
    putStrLn "give me sg to workaround with..."
    plainText <- getLine
    putStrLn $ "Caesar:  " ++ C.cipher c plainText
    putStrLn $ "Vigenere: " ++ C.cipher v plainText
