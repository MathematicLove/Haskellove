type Point = (Double, Double)
vertics :: [Point]
vertics = [(0, 0), (1, 0), (0.5, sqrt 3 / 2)]  
midpoint :: Point -> Point -> Point
midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)
sierpinski :: Int -> [Point] -> [[Point]]

sierpinski 0 triangle = [triangle]   
sierpinski n [p1, p2, p3] = 
    let m1 = midpoint p1 p2  
        m2 = midpoint p2 p3   
        m3 = midpoint p1 p3  
        lowerTriangles = sierpinski (n-1) [p1, m1, m3] ++
                         sierpinski (n-1) [m1, p2, m2] ++
                         sierpinski (n-1) [m3, m2, p3]
    in lowerTriangles

printSierpinski :: [[Point]] -> IO ()
printSierpinski [] = return ()
printSierpinski (p:ps) = do
    putStrLn $ show p ++ ","
    printSierpinski ps

main :: IO ()
main = do
    let initialTriangle = vertics   
    let depth = 6   
    let sierpinskiPoints = sierpinski depth initialTriangle   
    printSierpinski sierpinskiPoints   
