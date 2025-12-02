module Main where

import System.IO (readFile)
import Data.Char (isDigit)

-- | Checks if a number consists of a sequence of digits repeated at least twice.
isInvalidID :: Integer -> Bool
isInvalidID n =
    let s = show n
        len = length s

        divisors = [d | d <- [1 .. len `div` 2], len `rem` d == 0]
        
        check d =
            let pattern = take d s
                k = len `div` d
            in s == concat (replicate k pattern)
            
    -- If any valid divisor produces a matching repetition, the ID is invalid.
    in any check divisors

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delimiter str =
    let (start, rest) = break (== delimiter) str
    in start : case rest of
        []     -> []
        (_:xs) -> splitOn delimiter xs

-- | Parses a range string "min-max" into a tuple (min, max).
parseRange :: String -> (Integer, Integer)
parseRange str =
    let parts = splitOn '-' str
    in case parts of
        [a, b] -> (read a, read b)
        _      -> error $ "Invalid range format: " ++ str

cleanInput :: String -> String
cleanInput = filter (\c -> isDigit c || c == '-' || c == ',')

main :: IO ()
main = do
    content <- readFile "input.txt"
    
    let 
        cleanedContent = cleanInput content
        rangeStrings = splitOn ',' cleanedContent
        validRangeStrings = filter (not . null) rangeStrings
        ranges = map parseRange validRangeStrings
        
        invalidIDs = [ n | (start, end) <- ranges, n <- [start..end], isInvalidID n ]
        
        result = sum invalidIDs

    putStrLn $ "Sum of invalid IDs: " ++ show result