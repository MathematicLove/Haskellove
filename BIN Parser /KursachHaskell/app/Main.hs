module Main where
import Lib (parseAndEvaluateFile)

main :: IO ()
main = 
    putStrLn "Введите имя файла: " >>
    getLine >>= \filename ->
    parseAndEvaluateFile filename
