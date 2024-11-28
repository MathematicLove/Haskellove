{-# LANGUAGE LambdaCase #-}
module Lib (prodlojaNorm, nSlovarchik, saveSlovarchik, slovoKomputera, dialog) where
import Data.Char (isLetter, toLower)
import Data.List (nub, intercalate)
import Data.List.Split (splitWhen)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Random (randomRIO)

prodlojaNorm :: String -> Maybe [[String]]
prodlojaNorm text
    | null text = Nothing
    | otherwise = Just $ filter (not . null) $ map (words . normPredloja) $ predlojaS text
  where
    razdelit :: String
    razdelit = ".!?;:()"
    predlojaS :: String -> [String]
    predlojaS t = splitWhen (`elem` razdelit) t
    normPredloja :: String -> String
    normPredloja = map toLower . filter (\c -> isLetter c || c == ' ')

nSlovarchik :: Int -> [[String]] -> Map String [String]
nSlovarchik n predlojaS =
  Map.filter (not . null) $  
    Map.fromListWith (\new old -> nub (old ++ new)) $
      [ (unwords key, [value])
        | predlojNSlovar <- predlojaS
        , (key, value) <- slovarVPredloj n predlojNSlovar
      ]
  where
    slovarVPredloj :: Int -> [String] -> [([String], String)]
    slovarVPredloj n words =
      [ (key, unwords value)
        | i <- [0 .. length words - 1]
        , k <- [1 .. min (n - 1) (length words - i)]
        , let key = take k (drop i words)
        , let rest = drop (i + k) words
        , not (null rest)
        , l <- [1 .. min (n - k) (length rest)]
        , let value = take l rest
        , not (null value)  
      ]++
      [ (key, "")
        | i <- [max 0 (length words - (n - 1)) .. length words - 1], i >= 0
        , k <- [1 .. min (n - 1) (length words - i)]
        , let key = take k (drop i words)
        , length key == k
        , drop (i + k) words == []
      ]
saveSlovarchik :: FilePath -> Map String [String] -> IO ()
saveSlovarchik filePath dict =
    let vhod = Map.toAscList dict
        filterVhod = filter (not . null . snd) vhod  
        formattedvhod = map formatNorm filterVhod
    in writeFile filePath (unlines formattedvhod)
  where
    formatNorm (key, values) =
      let values' = nub values
          nePusto = if all null values' then [] else filter (not . null) values'
          valuesStr = intercalate ", " (map show nePusto)
      in key ++ " : [" ++ valuesStr ++ "]"

slovoKomputera :: Map String [String] -> [String] -> IO (Either String [String])
slovoKomputera dict keyWords = generate (unwords keyWords) [] maxLength
  where
    maxLength = 15
    minLength = 2
    generate _ acc 0
        | length acc >= minLength = return (Right (reverse acc))
        | otherwise = return (Left "Ошибкочка!")
    generate curKey acc n =
        case Map.lookup curKey dict of
            Nothing
                | length acc >= minLength -> return (Right (reverse acc))
                | otherwise -> return (Left $ "Завершен!")
            Just [] -> return (Left $ "Ключ '" ++ curKey ++ "' найден но продолжение отсутствует(")
            Just nextOptions ->
                randomRIO (0, length nextOptions - 1) >>= \idx ->
                let nextValue = nextOptions !! idx
                in if null nextValue
                   then return (Right (reverse acc))
                   else let continuation = words nextValue
                        in generate (unwords $ drop 1 (words curKey ++ continuation)) (continuation ++ acc) (n - 1)

keyPoUbiv :: [String] -> Map String [String] -> Maybe String
keyPoUbiv [] _ = Nothing
keyPoUbiv (x:xs) dict
    | Map.member x dict = Just x  
    | otherwise = keyPoUbiv xs dict  

dialog :: Map String [String] -> Map String [String] -> [String] -> Int -> IO ()
dialog dict1 dict2 initialPhrase steps = 
    let logDialog prevPhrase d1 d2 remainingSteps sobesNumber
            | remainingSteps == 0 = putStrLn "Диалог завершён"
            | otherwise = 
                let currentsobes = if sobesNumber `mod` 2 == 1 then 1 else 2
                    dict = if currentsobes == 1 then d1 else d2
                    otherDict = if currentsobes == 1 then d2 else d1
                    sobesLabel = "Чел " ++ show currentsobes ++ ": "
                in slovoKomputera dict prevPhrase >>= \case
                    Left err -> putStrLn (sobesLabel ++ err) >> putStrLn "Диалог завершён"
                    Right [] -> putStrLn (sobesLabel ++ "Ошибочка генерации!(") >> putStrLn "Диалог завершён"
                    Right generatedPhrase -> 
                        let phraseWithInitial = unwords prevPhrase ++ " " ++ unwords generatedPhrase
                            lastWords = reverse (words phraseWithInitial)
                        in putStrLn (sobesLabel ++ "(Ключ: " ++ unwords prevPhrase ++ ") " ++ phraseWithInitial) >>
                           case keyPoUbiv lastWords otherDict of
                               Nothing -> putStrLn "Ключ не найден !!!"
                               Just nextKey -> let nextPhrase = words nextKey
                                               in logDialog nextPhrase d1 d2 (remainingSteps - 1) (sobesNumber + 1)
    in logDialog initialPhrase dict1 dict2 steps 1

