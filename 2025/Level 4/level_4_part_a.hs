module Main where

import qualified Data.Set as S

-- | (Row, Column)
type Coord = (Int, Int)

type RollGrid = S.Set Coord

main :: IO ()
main = do
    content <- readFile "input.txt"
    let grid = parseInput content
    let result = solve grid
    putStrLn $ "Number of accessible rolls: " ++ show result

-- | Parse into a Set of coordinates
parseInput :: String -> RollGrid
parseInput input = S.fromList
    [ (r, c)
    | (r, line) <- zip [0..] (lines input)
    , (c, char) <- zip [0..] line
    , char == '@'
    ]

solve :: RollGrid -> Int
solve grid = S.size $ S.filter (isAccessible grid) grid

-- | Check if (has < 4 neighbors)
isAccessible :: RollGrid -> Coord -> Bool
isAccessible grid (r, c) = 
    let neighbors = countNeighbors grid (r, c)
    in neighbors < 4

countNeighbors :: RollGrid -> Coord -> Int
countNeighbors grid (r, c) = 
    length [ () 
           | dr <- [-1..1]
           , dc <- [-1..1]
           , not (dr == 0 && dc == 0) -- Skip the center (self)
           , S.member (r + dr, c + dc) grid 
           ]