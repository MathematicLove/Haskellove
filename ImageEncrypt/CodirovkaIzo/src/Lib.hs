module Lib (encryptTextInImage, decryptTextFromImage, saveBiographyFragment) where

import Codec.Picture
import Data.Bits
import Data.Char (ord, chr)
import Data.List (foldl', nub)
import Data.List.Split (splitOn)
import System.FilePath (takeBaseName, takeFileName)
import qualified Data.Vector.Unboxed as VU

saveBiographyFragment :: FilePath -> String -> IO ()
saveBiographyFragment path content = do
    let fragment = take 1000 content
        extendedFragment = if length fragment < length content
                           then takeWhile (/= '.') (drop (length fragment) content) ++ "."
                           else ""
        finalFragment = fragment ++ extendedFragment
    if length finalFragment < 1000
      then putStrLn "Ошибка: текст для биографии < 1000"
      else writeFile path finalFragment
originalAlphabet :: [Char]
originalAlphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)
generateSubstitutionAlphabet :: String -> Either String [Char]
generateSubstitutionAlphabet codeWord =
    if hasDuplicates codeWord
    then Left "Ошибка!! Символы должны отличаться((((("
    else Right (codeWord ++ [c | c <- originalAlphabet, c `notElem` codeWord])

encodeText :: String -> String -> Either String String
encodeText codeWord text = do
    subAlphabet <- generateSubstitutionAlphabet codeWord
    let alphabet = originalAlphabet
    Right [encodeChar c alphabet subAlphabet | c <- text]



encodeChar :: Char -> [Char] -> [Char] -> Char
encodeChar c alphabet subAlphabet =
    case lookup c (zip alphabet subAlphabet) of
        Just c' -> c'
        Nothing -> c  


encodeBits :: Char -> [Int]
encodeBits c = [ if testBit (ord c) i then 1 else 0 | i <- [7,6..0] ]
bitsToInt :: [Int] -> Int
bitsToInt bits = foldl' (\acc bit -> (acc `shiftL` 1) .|. bit) 0 bits
setMask :: Int -> Pixel8
setMask n = foldl' setBit 0 [0..(n - 1)]
clearMask :: Int -> Pixel8
clearMask n = complement (setMask n)
modifyBits :: Int -> Pixel8 -> Int -> Pixel8
modifyBits n originalByte bits =
    (originalByte .&. clearMask n) .|. (fromIntegral bits .&. setMask n)
encodeLength :: Int -> [Int]
encodeLength len = [ if testBit len i then 1 else 0 | i <- [31,30..0] ]
encryptBitsToImage :: Int -> Image PixelRGB8 -> [Int] -> Image PixelRGB8
encryptBitsToImage n img bitsData = generateImage encoder width height
  where
    width = imageWidth img
    height = imageHeight img
    totalBits = width * height * 3 * n
    bitsPaddedList = take totalBits (bitsData ++ repeat 0)
    bitsPadded = VU.fromList bitsPaddedList
    encoder x y =
      let index = x + y * width
          startPos = index * 3 * n
          pixelBits = VU.slice startPos (3 * n) bitsPadded
          bitsList = VU.toList pixelBits
          bitsR = bitsToInt $ take n bitsList
          bitsG = bitsToInt $ take n $ drop n bitsList
          bitsB = bitsToInt $ take n $ drop (2 * n) bitsList
          PixelRGB8 r g b = pixelAt img x y
          newR = modifyBits n r bitsR
          newG = modifyBits n g bitsG
          newB = modifyBits n b bitsB
      in PixelRGB8 newR newG newB

encryptTextInImage :: FilePath -> String -> String -> Int -> IO ()
encryptTextInImage imgPath text key bits = do
    putStrLn $ "Попытка чтения изображения: " ++ imgPath
    imgResult <- readImage imgPath
    case imgResult of
        Left err -> putStrLn $ "Ошибка изображения: " ++ err
        Right dynImg -> do
            putStrLn "Изображение прочитано"
            case encodeText key text of
                Left errMsg -> putStrLn errMsg
                Right encodedText -> do
                    let outputTextPath = "encrypted_image_text_" ++ key ++ ".txt"
                    writeFile outputTextPath encodedText
                    putStrLn $ "Зашифрованный текст сохранен в " ++ outputTextPath
                    let img = convertRGB8 dynImg
                        width = imageWidth img
                        height = imageHeight img
                        totalPixels = width * height
                        totalBitsAvailable = totalPixels * 3 * bits
                        maxChars = (totalBitsAvailable - 32) `div` 8
                    if length encodedText > maxChars
                        then putStrLn $ "Ошибка: текст слишком длинный (максимум " ++ show maxChars ++ " символов)"
                        else do
                            let messageLength = length encodedText
                            let lengthBits = encodeLength messageLength
                            let messageBits = concatMap encodeBits encodedText
                            let allBits = lengthBits ++ messageBits
                            let encodedImg = encryptBitsToImage bits img allBits
                            let outputImgPath = takeBaseName imgPath ++ "_encrypted_" ++ key ++ "_" ++ show bits ++ "bits.bmp"
                            putStrLn $ "Сохранение зашифрованного bmp: " ++ outputImgPath
                            saveBmpImage outputImgPath (ImageRGB8 encodedImg)
                            putStrLn "Зашифрованный файл сохранен"

extractBitsFromPixel :: Int -> PixelRGB8 -> [Int]
extractBitsFromPixel n (PixelRGB8 r g b) =
    let bitsR = [ if testBit r i then 1 else 0 | i <- [(n - 1),(n - 2)..0] ]
        bitsG = [ if testBit g i then 1 else 0 | i <- [(n - 1),(n - 2)..0] ]
        bitsB = [ if testBit b i then 1 else 0 | i <- [(n - 1),(n - 2)..0] ]
    in bitsR ++ bitsG ++ bitsB

decodeText :: String -> String -> Either String String
decodeText codeWord text = do
    subAlphabet <- generateSubstitutionAlphabet codeWord
    let alphabet = originalAlphabet
    Right [decodeChar c alphabet subAlphabet | c <- text]
decodeChar :: Char -> [Char] -> [Char] -> Char
decodeChar c alphabet subAlphabet =
    case lookup c (zip subAlphabet alphabet) of
        Just c' -> c'
        Nothing -> c   

decodeCharFromBits :: [Int] -> Char
decodeCharFromBits bits = chr $ bitsToInt bits
bitsToChars :: [Int] -> String
bitsToChars bits = [ decodeCharFromBits (take 8 (drop (i * 8) bits)) | i <- [0 .. (length bits `div` 8) - 1] ]

decodeTextFromImage :: Image PixelRGB8 -> Int -> String
decodeTextFromImage img bits = decodedMessage
  where
    width = imageWidth img
    height = imageHeight img
    pixels = [ pixelAt img x y | y <- [0..height -1], x <- [0..width -1 ] ]
    extractedBits = concatMap (extractBitsFromPixel bits) pixels
    (lengthBits, restBits) = splitAt 32 extractedBits
    messageLength = bitsToInt lengthBits
    messageBits = take (messageLength * 8) restBits
    decodedMessage = bitsToChars messageBits

decryptTextFromImage :: FilePath -> IO ()
decryptTextFromImage imgPath = do
    let fileName = takeFileName imgPath
        parts = splitOn "_" fileName
        key = if length parts >= 3 then parts !! 2 else error "Ключ не найден в имени файла"
        bitsStr = if length parts >= 4 then parts !! 3 else error "Количество битов не указано"
        bits = read (filter (`elem` ['0'..'9']) bitsStr) :: Int
    putStrLn $ "Ключ: " ++ key
    putStrLn $ "Количество битов: " ++ show bits
    imgResult <- readImage imgPath
    case imgResult of
        Left err -> putStrLn $ "Ошибка изображения: " ++ err
        Right dynImg -> do
            putStrLn "Изображение готово"
            let img = convertRGB8 dynImg
                decodedText = decodeTextFromImage img bits
            case decodeText key decodedText of
                Left errMsg -> putStrLn errMsg
                Right finalText -> do
                    let outputPath = takeBaseName imgPath ++ "_decrypted.txt"
                    writeFile outputPath finalText
                    putStrLn $ "Расшифровка готова, текст сохранен в " ++ outputPath
