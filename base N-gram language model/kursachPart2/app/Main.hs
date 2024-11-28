{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use >=>" #-}
module Main where
import Lib
import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as Map

main :: IO ()
main = 
    putStrLn "Какую задачку рассмотрим?" >>
    putStrLn "1 - Разделить текст на предл." >>
    putStrLn "2 - Словарь N-грамм" >>
    putStrLn "3 - Сгенерировать предложение" >>
    putStrLn "4 - Диалог двух челов" >>
    hFlush stdout >>
    getLine >>= \choice ->
    case choice of
        "1" -> part1
        "2" -> part2
        "3" -> part3
        "4" -> part4
        _   -> putStrLn "Выберите из цифр, которые в меню" >> main
part1 :: IO ()
part1 =
    putStrLn "Введите имя файла:" >>
    hFlush stdout >>
    getLine >>= \filename ->
    readFile filename >>= \content ->
    case prodlojaNorm content of
        Nothing -> putStrLn "Тут пусто ("
        Just sentences -> putStrLn "Результат:" >> mapM_ print sentences
part2 :: IO ()
part2 =
    putStrLn "Введите имя файла для словаря:" >>
    hFlush stdout >>
    getLine >>= \filename ->
    readFile filename >>= \content ->
    case prodlojaNorm content of
        Nothing -> putStrLn "Тут пусто ("
        Just sentences ->
            putStrLn "Введите N-грамму:" >>
            hFlush stdout >>
            getLine >>= \nInput ->
            let n = read nInput :: Int
                dict = nSlovarchik n sentences
                dictFileName = show n ++ "_gram_slovar.txt" ++ filename
            in if Map.null dict
               then putStrLn "Ошибка!("
               else saveSlovarchik dictFileName dict >>
                    putStrLn "Ура работает!" >>
                    putStrLn ("Словарь сохранён: " ++ dictFileName) >>
                    putStrLn "Оп оп:" >>
                    readFile dictFileName >>= putStrLn
part3 :: IO ()
part3 =
    putStrLn "Введите имя файла для словаря:" >>
    hFlush stdout >>
    getLine >>= \filename ->
    readFile filename >>= \content ->
    case prodlojaNorm content of
        Nothing -> putStrLn "Тут пусто ("
        Just sentences ->
            putStrLn "Введите N-грамму:" >>
            hFlush stdout >>
            getLine >>= \nInput ->
            let n = read nInput :: Int
                dict = nSlovarchik n sentences
            in if Map.null dict
               then putStrLn "Упс ! Невозможно построить так!"
               else putStrLn "Введите слово/ предлож. для генерации:" >>
                    hFlush stdout >>
                    getLine >>= \input ->
                    let keyWords = words input
                        key = unwords keyWords
                    in case Map.lookup key dict of
                        Nothing -> putStrLn ("Упс! Ключа '" ++ key ++ "' нет в словаре ((")
                        Just _ -> slovoKomputera dict keyWords >>= \case
                            Left err -> putStrLn err
                            Right phrase -> putStrLn ("Сгенерированная фраза: " ++ unwords (keyWords ++ phrase))
part4 :: IO ()
part4 =
    putStrLn "Введите имя первого файла (1):" >>
    hFlush stdout >>
    getLine >>= \file1 ->
    putStrLn "Введите имя второго файла (2):" >>
    hFlush stdout >>
    getLine >>= \file2 ->
    readFile file1 >>= \content1 ->
    readFile file2 >>= \content2 ->
    case (prodlojaNorm content1, prodlojaNorm content2) of
        (Nothing, _) -> putStrLn "Упс! Пусто в первом файле!"
        (_, Nothing) -> putStrLn "Упс! Пусто во втором файле!"
        (Just sentences1, Just sentences2) ->
            putStrLn "Введите N-грамму:" >>
            hFlush stdout >>
            getLine >>= \nInput ->
            let n = read nInput :: Int
                dict1 = nSlovarchik n sentences1
                dict2 = nSlovarchik n sentences2
            in putStrLn "Введите начальное слово или предложение:" >>
               hFlush stdout >>
               getLine >>= \input ->
               let initialKey = words input
               in putStrLn "Введите глубину диалога:" >>
                  hFlush stdout >>
                  getLine >>= \mInput ->
                  let m = read mInput :: Int
                  in dialog dict1 dict2 initialKey m
