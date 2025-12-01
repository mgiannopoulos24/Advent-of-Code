module Main where

import System.IO

-- State: (Current Position, Total Hits)
-- Position is always 0-99.
type State = (Int, Int)

parse :: String -> (Char, Int)
parse s = (head s, read (tail s))

processStep :: State -> (Char, Int) -> State
processStep (pos, hits) (dir, val) = (newPos, hits + newHits)
  where
    dialSize = 100
    
    -- 1. Count hits from full loops.
    -- Since the dial has 100 numbers, every 100 clicks hits '0' exactly once.
    fullLoops = val `div` dialSize
    
    -- 2. Count hits from the remaining partial loop.
    remainder = val `mod` dialSize
    
    remainderHit = case dir of
        -- Turning Right (Increasing):
        'R' -> if (pos + remainder) >= dialSize then 1 else 0
        
        -- Turning Left (Decreasing):
        'L' -> if pos > 0 && (pos - remainder) <= 0 then 1 else 0
        
        _   -> 0
        
    newHits = fullLoops + remainderHit
    
    -- Calculate new position for the next step
    newPos = case dir of
        'R' -> (pos + val) `mod` dialSize
        'L' -> (pos - val) `mod` dialSize
        _   -> pos

main :: IO ()
main = do
    content <- readFile "input.txt"
    let instructions = map parse (lines content)
    
    -- The dial starts at 50, with 0 hits initially.
    let initialState = (50, 0)
    
    let (_, totalHits) = foldl processStep initialState instructions
    
    putStrLn $ "The Part Two password is: " ++ show totalHits