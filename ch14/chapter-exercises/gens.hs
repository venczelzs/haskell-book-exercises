-- gens.hs

module Gens where

import Test.QuickCheck
import Test.QuickCheck.Gen

data Fool = Fulse | Frue deriving (Eq, Show)

genFoolEqDist :: Gen Fool
genFoolEqDist = arbitrary

genFoolIneqDist :: Gen Fool
genFoolIneqDist = do
  frequency [ (3, return Fulse), (1, return Frue) ]

instance Arbitrary Fool where
  arbitrary = elements [Frue, Fulse]

main :: IO ()
main = do
  putStrLn "equal probability"
  sample genFoolEqDist
  putStrLn "2/3 fulse, 1/3 frue"
  sample genFoolIneqDist
