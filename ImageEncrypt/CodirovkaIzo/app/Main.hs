module Main where
import System.Environment (getArgs)
import Lib (encryptTextInImage, decryptTextFromImage,
            saveBiographyFragment)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["encrypt", imgPath, textFile, key, bitsStr] -> do
            text <- readFile textFile
            saveBiographyFragment "biography1000.txt" text
            let bits = read bitsStr :: Int
            encryptTextInImage imgPath text key bits
            putStrLn "Шифрование готово"
        ["decrypt", imgPath] -> do
            decryptTextFromImage imgPath
            putStrLn "Расшифровка завершена. Текст сохранен в txt"
        _ -> putStrLn "Выберите: \n\
                      \ Для шифрования: encrypt \
                      \ \n\
                      \ Для расшифровки: decrypt"
