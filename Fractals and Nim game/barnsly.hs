type Point = (Double, Double)

maxIterations :: Int
maxIterations = 1000

barnsleyFern :: Point -> Int -> Point
barnsleyFern (x, y) n
    | r == 0 = (0, 0.16 * y)
    | r <= 85 = (0.85 * x + 0.04 * y, -0.04 * x + 0.85 * y + 1.6)
    | r <= 92 = (0.2 * x - 0.26 * y, 0.23 * x + 0.22 * y + 1.6)
    | otherwise = (-0.15 * x + 0.28 * y, 0.26 * x + 0.24 * y + 0.44)
  where
    r = n `mod` 100 

generateBarnsleyFern :: Point -> Int -> [[Point]]
generateBarnsleyFern _ 0 = []
generateBarnsleyFern p n = 
    let nextPoint = barnsleyFern p n
        pointsInStep = replicate 4 nextPoint 
    in [pointsInStep] ++ generateBarnsleyFern nextPoint (n - 1)

printBarnsleyFern :: [[Point]] -> IO ()
printBarnsleyFern [] = return ()
printBarnsleyFern (p:ps) = do
    putStrLn $ show p ++ ","
    printBarnsleyFern ps

main :: IO ()
main = do
    let points = generateBarnsleyFern (0, 0) (maxIterations `div` 4)  
    printBarnsleyFern points
