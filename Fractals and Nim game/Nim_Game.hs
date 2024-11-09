import Data.List(intercalate) 

mInitial :: Int  
mInitial = 50
kMax :: Int  
kMax = 10

type Move = Int
type GameState = Int
type MoveHistory = [(String, Move)]  

nashStrategy :: GameState -> Move
nashStrategy m
  | remainder == 0 = 1  
  | otherwise = remainder
  where remainder = m `mod` (kMax + 1)

moyaStrategiya :: GameState -> IO Move
moyaStrategiya m = do
  putStrLn $ "Осталось " ++ show m ++ " камней"
  putStrLn $ "Ваш ход: сколько камней вы возьмете? (1 до " ++ show (min kMax m) ++ ")"
  move <- getLine
  let move' = read move :: Int
  if move' > 0 && move' <= min kMax m
     then return move'
     else do
       putStrLn "Неверный ход!"
       moyaStrategiya m

compik :: GameState -> IO Move
compik m = do
  let move = nashStrategy m
  putStrLn $ "Компьютер взял " ++ show move ++ " камней."
  return move

isGameOver :: GameState -> Bool
isGameOver = (== 0)

printMoves :: MoveHistory -> IO ()
printMoves moves = do
  putStrLn "Ходы в игре:"
  putStrLn $ intercalate "\n" (map (\(player, move) -> player ++ " взяли " ++ show move ++ " камней.") moves)

playNim :: GameState -> MoveHistory -> IO ()
playNim m moves
  | isGameOver m = do
      putStrLn "Game Over"
      printMoves moves
  | otherwise = do
      hodUser <- moyaStrategiya m
      let m' = m - hodUser
      let moves' = moves ++ [("Вы", hodUser)]
      if isGameOver m'
         then do
           putStrLn "Вы выиграли! Ура!"
           printMoves moves'
           putStrLn "Счет: 1-0 в вашу пользу."
         else do
           hodComp <- compik m'
           let m'' = m' - hodComp
           let moves'' = moves' ++ [("Компьютер", hodComp)]
           if isGameOver m''
              then do
                putStrLn "Компьютер выиграл(("
                printMoves moves''
                putStrLn "Счет: 0-1 в пользу компьютера."
              else playNim m'' moves''

main :: IO ()
main = do
  putStrLn "Ним игра"
  playNim mInitial []
