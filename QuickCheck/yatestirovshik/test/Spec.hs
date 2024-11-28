module Main (main) where
import Test.QuickCheck
import Lib (multiplyMod, myGcd)

prop_multiplyMod_mod :: Int -> Int -> Int -> Property
prop_multiplyMod_mod x y m = m > 0 ==> (multiplyMod x y m) `mod` m == (x * y) `mod` m
prop_multiplyMod_identity :: Int -> Int -> Property
prop_multiplyMod_identity x m = m > 0 ==> multiplyMod x 1 m == x `mod` m
prop_multiplyMod_commutative :: Int -> Int -> Int -> Property
prop_multiplyMod_commutative x y m = m > 0 ==> multiplyMod x y m == multiplyMod y x m

prop_myGcd_self :: Int -> Bool
prop_myGcd_self x = myGcd x x == abs x
prop_myGcd_one :: Int -> Bool
prop_myGcd_one x = myGcd x 1 == 1
prop_myGcd_commutative :: Int -> Int -> Bool
prop_myGcd_commutative a b = myGcd a b == myGcd b a
prop_myGcd_associative :: Int -> Int -> Int -> Bool
prop_myGcd_associative a b c = myGcd (myGcd a b) c == myGcd a (myGcd b c)

main :: IO ()
main = do
  putStrLn "Тестики для умножения по модулю:"
  quickCheck prop_multiplyMod_mod
  quickCheck prop_multiplyMod_identity
  quickCheck prop_multiplyMod_commutative
  putStrLn "Тестики для НОД:"
  quickCheck prop_myGcd_self
  quickCheck prop_myGcd_one
  quickCheck prop_myGcd_commutative
  quickCheck prop_myGcd_associative
  -- let nTestov = stdArgs {maxSuccess = 5993}
  -- quickCheckWith nTestov prop_multiplyMod_commutative
