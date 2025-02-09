module Lib (parseAndEvaluateFile) where
import Control.Applicative (Alternative(..))
import Data.Char (digitToInt, isDigit)

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
  
eof :: Parser tok ()
eof = Parser $ \input -> if null input then Just (input, ()) else Nothing

char :: Eq tok => tok -> Parser tok tok
char c = satisfy (== c)

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

spaces :: Parser Char ()
spaces = () <$ many (satisfy (== ' '))

number :: Parser Char Int
number = read <$> some (satisfy isDigit)

opParser :: Parser Char (Int -> Int -> Bool)
opParser = (char '>' *> pure (>)) <|> (char '<' *> pure (<)) <|> (char '=' *> pure (==))

expression :: Parser Char Bool
expression = do
  num1 <- number
  spaces
  op <- opParser
  spaces
  num2 <- number
  return (op num1 num2)

parseAndEvaluate :: String -> Either String Bool
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
      Right result -> line ++ " - " ++ (if result then "true" else "false")
