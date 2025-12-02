module Main where

import System.IO (readFile)
import Data.Char (isDigit)

isInvalidID :: Integer -> Bool
isInvalidID n =
    let s = show n
        len = length s
        (halfLen, remainder) = len `divMod` 2
        (firstHalf, secondHalf) = splitAt halfLen s
    in remainder == 0 && firstHalf == secondHalf

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

-- | Cleans the input string by keeping only digits, dashes, and commas.
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
        
        -- Generate all numbers for each range and filter for Invalid IDs.
        invalidIDs = [ n | (start, end) <- ranges, n <- [start..end], isInvalidID n ]
        
        result = sum invalidIDs

    putStrLn $ "Sum of invalid IDs: " ++ show result