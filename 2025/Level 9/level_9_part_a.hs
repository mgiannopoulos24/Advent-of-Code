module Main where

import Data.List (tails)

parsePoint :: String -> (Int, Int)
parsePoint str = 
    let (xStr, rest) = span (/= ',') str
        
        yStr = drop 1 rest
    in (read xStr, read yStr)

calculateArea :: (Int, Int) -> (Int, Int) -> Int
calculateArea (x1, y1) (x2, y2) = 
    (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

solve :: [(Int, Int)] -> Int
solve points = maximum $ 0 : [calculateArea p1 p2 | (p1:ps) <- tails points, p2 <- (p1:ps)]

main :: IO ()
main = do
    content <- readFile "input.txt"
    
    let points = map parsePoint $ filter (not . null) $ lines content
    
    putStrLn $ "The largest area is: " ++ show (solve points)