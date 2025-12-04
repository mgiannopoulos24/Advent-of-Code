module Main where

import qualified Data.Set as S

-- | (Row, Column)
type Coord = (Int, Int)

type RollGrid = S.Set Coord

main :: IO ()
main = do
    content <- readFile "input.txt"
    let initialGrid = parseInput content
    let totalRemoved = simulateRemoval initialGrid 0
    putStrLn $ "Total rolls removed: " ++ show totalRemoved

-- | Parse into a Set of coordinates
parseInput :: String -> RollGrid
parseInput input = S.fromList
    [ (r, c)
    | (r, line) <- zip [0..] (lines input)
    , (c, char) <- zip [0..] line
    , char == '@'
    ]

simulateRemoval :: RollGrid -> Int -> Int
simulateRemoval grid count =
    let 
        -- Identify all rolls (neighbors < 4)
        toRemove = S.filter (isAccessible grid) grid
        numToRemove = S.size toRemove
    in
        if numToRemove == 0
        then count 
        else 
            let 
                newGrid = S.difference grid toRemove
            in 
                simulateRemoval newGrid (count + numToRemove)

-- | Check if has < 4 neighbors
isAccessible :: RollGrid -> Coord -> Bool
isAccessible grid coord = 
    countNeighbors grid coord < 4

countNeighbors :: RollGrid -> Coord -> Int
countNeighbors grid (r, c) = 
    length [ () 
           | dr <- [-1..1]
           , dc <- [-1..1]
           , not (dr == 0 && dc == 0) -- Skip the center (self)
           , S.member (r + dr, c + dc) grid 
           ]