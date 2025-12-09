module Main where

import System.IO
import Data.List (tails)

type Point = (Int, Int)
type Edge = (Point, Point)

parsePoint :: String -> Point
parsePoint str = 
    let (xStr, rest) = span (/= ',') str
        yStr = drop 1 rest
    in (read xStr, read yStr)

calculateArea :: Point -> Point -> Int
calculateArea (x1, y1) (x2, y2) = 
    (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

strictlyBetween :: Int -> Int -> Int -> Bool
strictlyBetween x a b = x > min a b && x < max a b

intervalsOverlap :: Int -> Int -> Int -> Int -> Bool
intervalsOverlap a1 a2 b1 b2 = 
    max (min a1 a2) (min b1 b2) < min (max a1 a2) (max b1 b2)

intersectsInterior :: Point -> Point -> Edge -> Bool
intersectsInterior (rx1, ry1) (rx2, ry2) ((ex1, ey1), (ex2, ey2))
    | ex1 == ex2 = 
        strictlyBetween ex1 rx1 rx2 && intervalsOverlap ey1 ey2 ry1 ry2
    | ey1 == ey2 = 
        strictlyBetween ey1 ry1 ry2 && intervalsOverlap ex1 ex2 rx1 rx2
    | otherwise = False 

isInside :: Point -> Point -> [Edge] -> Bool
isInside (x1, y1) (x2, y2) edges =
    let mx = fromIntegral (x1 + x2) / 2.0 :: Double
        my = fromIntegral (y1 + y2) / 2.0 :: Double
        
        onBoundary ((ex1, ey1), (ex2, ey2)) =
            let (dx1, dy1) = (fromIntegral ex1, fromIntegral ey1)
                (dx2, dy2) = (fromIntegral ex2, fromIntegral ey2)
                minX = min dx1 dx2; maxX = max dx1 dx2
                minY = min dy1 dy2; maxY = max dy1 dy2
            in (dx1 == dx2 && abs (mx - dx1) < 1e-9 && my >= minY && my <= maxY) ||
               (dy1 == dy2 && abs (my - dy1) < 1e-9 && mx >= minX && mx <= maxX)
        
        onB = any onBoundary edges
        
        rayY = my + 1e-5
        crossesRay ((ex1, ey1), (ex2, ey2)) =
            let yA = fromIntegral ey1; yB = fromIntegral ey2
                xA = fromIntegral ex1; xB = fromIntegral ex2
            in if (yA > rayY) /= (yB > rayY)
               then
                   let intersectX = xA + (rayY - yA) * (xB - xA) / (yB - yA)
                   in intersectX > mx
               else False
               
        intersections = length (filter crossesRay edges)
    in onB || odd intersections

isValid :: Point -> Point -> [Edge] -> Bool
isValid p1 p2 edges = 
    let boundaryOk = not (any (intersectsInterior p1 p2) edges)
    in boundaryOk && isInside p1 p2 edges

solve :: [Point] -> Int
solve points = 
    let edges = zip points (tail points ++ [head points])
        pairs = [(p1, p2) | (p1:ps) <- tails points, p2 <- (p1:ps)]
    in maximum $ 0 : [calculateArea p1 p2 | (p1, p2) <- pairs, isValid p1 p2 edges]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let points = map parsePoint $ filter (not . null) $ lines content
    putStrLn $ "The largest area is: " ++ show (solve points)