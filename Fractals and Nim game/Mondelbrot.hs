type Point = (Double, Double)

maxIt :: Int
maxIt = 10

mandelBROt :: Point -> Int -> Bool
mandelBROt (x, y) maxIter = mandelBROtIter (0, 0) (x, y) maxIter 0

mandelBROtIter :: (Double, Double) -> (Double, Double) -> Int -> Int -> Bool
mandelBROtIter (zx, zy) (cx, cy) maxIter iter
    | zx * zx + zy * zy > 4 = False   
    | iter >= maxIter       = True   
    | otherwise = mandelBROtIter (newZx, newZy) (cx, cy) maxIter (iter + 1)
  where
    newZx = zx * zx - zy * zy + cx
    newZy = 2 * zx * zy + cy

mandelBRO :: Double -> Double -> Double -> Double -> Double -> Double -> [[Point]]
mandelBRO minX maxX minY maxY stepX stepY =
    [filter (not . null) [(x, y) 
    | x <- [minX, minX + stepX .. maxX]
    , mandelBROt (x, y) maxIt] 
    | y <- [minY, minY + stepY .. maxY], not (null [x | x <- [minX, minX + stepX .. maxX], mandelBROt (x, y) maxIt])]

printmandelBROtSet :: [[Point]] -> IO ()
printmandelBROtSet points = mapM_ printWithComma points
  where
    printWithComma :: [Point] -> IO ()
    printWithComma ps = putStrLn $ show ps ++ ","

main :: IO ()
main = do
    let mandelBROtSet = mandelBRO (-2.0) 1.0 (-1.5) 1.5 0.05 0.05
    printmandelBROtSet mandelBROtSet
