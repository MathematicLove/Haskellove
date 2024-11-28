module Lib (multiplyMod, myGcd) where

multiplyMod :: Int -> Int -> Int -> Int
multiplyMod x y m = (x * y) `mod` m

myGcd :: Int -> Int -> Int
myGcd a b
  | b == 0 = abs a
  | otherwise = myGcd b (a `mod` b)
