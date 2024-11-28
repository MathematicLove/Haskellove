{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module Lib (parseAndEvaluateFile) where
import Control.Applicative (Alternative(..))
import Data.Char (digitToInt)
newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok], a) }

instance Functor (Parser tok) where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Nothing -> Nothing
      Just (rest, result) -> Just (rest, f result)

instance Applicative (Parser tok) where
  pure x = Parser $ \input -> Just (input, x)
  Parser pf <*> Parser px = Parser $ \input ->
    case pf input of
      Nothing -> Nothing
      Just (rest1, f) -> case px rest1 of
        Nothing -> Nothing
        Just (rest2, x) -> Just (rest2, f x)

instance Monad (Parser tok) where
  (>>=) :: Parser tok a -> (a -> Parser tok b) -> Parser tok b
  Parser p >>= f = Parser $ \input ->
    case p input of
      Nothing -> Nothing
      Just (rest, result) -> runParser (f result) rest

instance Alternative (Parser tok) where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Nothing -> p2 input
      result -> result

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser $ \input -> case input of
  (c:cs) | pr c -> Just (cs, c)
  _ -> Nothing

char :: Eq tok => tok -> Parser tok tok
char c = satisfy (== c)

digit :: Parser Char Int
digit = digitToInt <$> satisfy (`elem` "01")

spaces :: Parser Char ()
spaces = () <$ many (satisfy (== ' '))

bitString :: Parser Char [Int]
bitString = spaces *> some digit <* spaces

virovS :: [Int] -> [Int] -> ([Int], [Int])
virovS a b =
    let maxLength = max (length a) (length b)
        padLeft xs = replicate (maxLength - length xs) 0 ++ xs
    in (padLeft a, padLeft b)

bitAnd, bitOr, bitXor :: [Int] -> [Int] -> [Int]
bitAnd a b = let (x, y) = virovS a b in zipWith (\x y -> if x == 1 && y == 1 then 1 else 0) x y
bitOr  a b = let (x, y) = virovS a b in zipWith (\x y -> if x == 1 || y == 1 then 1 else 0) x y
bitXor a b = let (x, y) = virovS a b in zipWith (\x y -> if x /= y then 1 else 0) x y

chainl1 :: Parser tok a -> Parser tok (a -> a -> a) -> Parser tok a
chainl1 p op = p >>= rest
  where
    rest x = (op <*> pure x <*> p >>= rest) <|> pure x
eof :: Parser tok ()
eof = Parser $ \input -> if null input then Just (input, ()) else Nothing

expression :: Parser Char [Int]
expression = chainl1 bitString opParser

opParser :: Parser Char ([Int] -> [Int] -> [Int])
opParser =
      (bitAnd <$ char '&')
  <|> (bitOr  <$ char '|')
  <|> (bitXor <$ char '^')

parseAndEvaluate :: String -> Either String [Int]
parseAndEvaluate str =
  case runParser (expression <* eof) str of
    Nothing -> Left "Ошибка парсинга!"
    Just ("", result) -> Right result
    Just _ -> Left "Не удалось разобрать всю строку("

parseAndEvaluateFile :: FilePath -> IO ()
parseAndEvaluateFile filename =
  readFile filename >>= mapM_ putStrLn . processFile . lines
  where
    processFile :: [String] -> [String]
    processFile = map evaluateLine
    evaluateLine :: String -> String
    evaluateLine line = case parseAndEvaluate line of
      Left err -> "Ошибка: " ++ err
      Right result -> line ++ " = " ++ concatMap show result