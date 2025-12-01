module Main where

import System.IO

-- The dial ranges from 0 to 99, so we use modulo 100 arithmetic.
dialSize :: Int
dialSize = 100

startPos :: Int
startPos = 50


move :: Int -> String -> Int
move currentPos instruction =
    let direction = head instruction
        amountStr = tail instruction
        amount    = read amountStr :: Int
    in case direction of
        'R' -> (currentPos + amount) `mod` dialSize -- Right adds value
        'L' -> (currentPos - amount) `mod` dialSize -- Left subtracts value
        _   -> currentPos -- Should not happen with valid input

main :: IO ()
main = do
    content <- readFile "input.txt"
    let instructions = lines content


    let history = scanl move startPos instructions

    let positionsAfterMoves = tail history

    -- Count how many times the position is exactly 0
    let password = length $ filter (== 0) positionsAfterMoves

    putStrLn $ "The actual password is: " ++ show password