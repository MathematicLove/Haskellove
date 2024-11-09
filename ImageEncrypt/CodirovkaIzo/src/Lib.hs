module Lib (encryptTextInImage, decryptTextFromImage, saveBiographyFragment, encryptTextWithBits) where
import Codec.Picture
import Data.Bits
import Data.Char (ord, chr)
import Data.List (foldl', isSuffixOf)
import Data.List.Split (splitOn)
import System.FilePath (takeBaseName, takeFileName)
import Data.List (foldl', isSuffixOf, elemIndex)
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

encryptTextWithBits :: Int -> Image PixelRGB8 -> String -> String -> Image PixelRGB8
encryptTextWithBits n img text key = generateImage encoder width height
  where
    width = imageWidth img
    height = imageHeight img
    combinedBits = concatMap encodeBits (text ++ key)
    totalBits = width * height * 3 * n
    bitsPaddedList = take totalBits (combinedBits ++ repeat 0)
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
            let img = convertRGB8 dynImg
                width = imageWidth img
                height = imageHeight img
                totalPixels = width * height
                totalBitsAvailable = totalPixels * 3 * bits
                maxChars = totalBitsAvailable `div` 8
                textWithKey = text ++ key
            if length textWithKey > maxChars
                then putStrLn $ "Ошибка: текст слишком длинный( " ++ show maxChars
                else do
                    let encodedImg = encryptTextWithBits bits img text key
                        outputPath = takeBaseName imgPath ++ "_encrypted_" ++ key ++ "_" ++ show bits ++ "bits.bmp"
                    putStrLn $ "Сохранение зашифрованного bmp: " ++ outputPath
                    saveBmpImage outputPath (ImageRGB8 encodedImg)
                    putStrLn "Зашифрованный файл сохранен"

extractBitsFromPixel :: Int -> PixelRGB8 -> [Int]
extractBitsFromPixel n (PixelRGB8 r g b) =
    let bitsR = [ if testBit r i then 1 else 0 | i <- [(n - 1),(n - 2)..0] ]
        bitsG = [ if testBit g i then 1 else 0 | i <- [(n - 1),(n - 2)..0] ]
        bitsB = [ if testBit b i then 1 else 0 | i <- [(n - 1),(n - 2)..0] ]
    in bitsR ++ bitsG ++ bitsB

decodeChar :: [Int] -> Char
decodeChar bits = chr $ bitsToInt bits

decodeTextFromImage :: Image PixelRGB8 -> String -> Int -> String
decodeTextFromImage img key bits = decodeChars bitsStreamLimited "" key keyLength
  where
    width = imageWidth img
    height = imageHeight img
    keyLength = length key
    maxChars = 1000 + keyLength
    pixels = [ pixelAt img x y | y <- [0..height - 1], x <- [0..width - 1] ]
    extractedBits = concatMap (extractBitsFromPixel bits) pixels
    bitsStreamLimited = take (maxChars * 8 * 2) extractedBits   

decodeChars :: [Int] -> String -> String -> Int -> String
decodeChars bitsLeft decodedText key keyLength
  | length decodedText >= keyLength && key `isSuffixOf` decodedText =
      let truncatedText = take (length decodedText - keyLength) decodedText
          lastFullStopIndex = findLastFullStop truncatedText
      in if lastFullStopIndex /= -1
         then take (lastFullStopIndex + 1) truncatedText
         else truncatedText
  | length bitsLeft < 8 = decodedText
  | otherwise =
      let (charBits, restBits) = splitAt 8 bitsLeft
          c = decodeChar charBits
      in decodeChars restBits (decodedText ++ [c]) key keyLength

findLastFullStop :: String -> Int
findLastFullStop txt =
  let (initialPart, remainingPart) = splitAt 1000 txt
  in case elemIndex '.' remainingPart of
       Just idx -> 1000 + idx
       Nothing  -> -1

findIndexRev :: (a -> Bool) -> [a] -> Maybe Int
findIndexRev p xs = go (reverse xs) (length xs - 1)
  where
    go [] _ = Nothing
    go (y:ys) idx
      | p y       = Just idx
      | otherwise = go ys (idx - 1)

decryptTextFromImage :: FilePath -> IO ()
decryptTextFromImage imgPath = do
    let fileName = takeFileName imgPath
        parts = splitOn "_" fileName
        key = if length parts >= 3 then parts !! 2 else error "Ключ не тот"
        bitsStr = if length parts >= 4 then parts !! 3 else error "Количество битов нет"
        bits = read (filter (`elem` ['0'..'9']) bitsStr) :: Int
    putStrLn $ "Ключ: " ++ key
    putStrLn $ "Количество битов: " ++ show bits
    imgResult <- readImage imgPath
    case imgResult of
        Left err -> putStrLn $ "Ошибка изображения: " ++ err
        Right dynImg -> do
            putStrLn "Изображение готово"
            let img = convertRGB8 dynImg
                decodedText = decodeTextFromImage img key bits
                outputPath = takeBaseName imgPath ++ "_decrypted.txt"
            writeFile outputPath decodedText
            putStrLn $ "Расшифровка готова, текст сохранен в " ++ outputPath
