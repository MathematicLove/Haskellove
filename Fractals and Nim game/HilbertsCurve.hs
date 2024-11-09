type Point = (Double, Double)   
type LevelPoints = [[Point]]

hilbertC :: Int -> [[Point]]
hilbertC 0 = [[(0, 0)]]
hilbertC n = 
    let previousLevels = hilbertC (n - 1)
        levelNow = naNext (fromIntegral (2 ^ (n - 1))) (last previousLevels)  
    in previousLevels ++ [levelNow]
scalePoints :: Double -> [Point] -> [[Point]]
scalePoints maxCoord points = 
    let scaled = [(x / maxCoord, y / maxCoord) | (x, y) <- points]
    in spSp scaled
spSp :: [Point] -> [[Point]]
spSp points = 
    let maxX = maximum [x | (x, _) <- points]
        maxY = maximum [y | (_, y) <- points]
    in [[(x, y) | (x, y) <- points, y == j] | j <- [0..maxY], x <- [0..maxX]]
naNext :: Double -> [Point] -> [Point]
naNext size points =
    let 
        tLeft  = [(y, x) | (x, y) <- points]
        tRight = [(x + size, y) | (x, y) <- points]
        bRight = [(x + size, y + size) | (x, y) <- points]
        bLeft  = [(size - 1 - y, size - 1 - x + size) | (x, y) <- points]
    in tLeft ++ tRight ++ bRight ++ bLeft
printSLHilbert :: Int -> IO ()
printSLHilbert n = 
    let hilbertPoints = last (hilbertC n)
        maxCoord = fromIntegral (2 ^ n - 1)   
        scaledPoints = scalePoints maxCoord hilbertPoints
    in print scaledPoints

main :: IO ()
main = printSLHilbert 5