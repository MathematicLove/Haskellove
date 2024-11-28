module Main where
import Lib (multiplyMod, myGcd)

main :: IO ()
main = do
  putStrLn "Умнож. по модулю:"
  putStrLn $ "3 4 5 = " ++ show (multiplyMod 3 4 5)
  putStrLn $ "10 15 7 = " ++ show (multiplyMod 10 15 7)
  putStrLn $ "20 30 6 = " ++ show (multiplyMod 20 30 6)
  putStrLn "\nНОД:"
  putStrLn $ "30 45 = " ++ show (myGcd 30 45)
  putStrLn $ "17 23 = " ++ show (myGcd 17 23)
  putStrLn $ "100 25 = " ++ show (myGcd 100 25)
