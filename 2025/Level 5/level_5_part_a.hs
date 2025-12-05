module Main where

import System.Environment (getArgs)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (isSpace)

type Range = (Int, Int)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parseRange :: String -> Range
parseRange s =
  case span (/= '-') (trim s) of
    (a, '-' : b) -> (read a, read b)
    _            -> error $ "invalid range: " ++ s

mergeRanges :: [Range] -> [Range]
mergeRanges rs = foldr insertSorted [] (sortBy (comparing fst) rs)
  where
    insertSorted r [] = [r]
    insertSorted (a,b) acc@((c,d):rest)
      | b < c - 1  = (a,b) : acc                        
      | a > d + 1  = (a,b) : acc                       
      | otherwise  = mergeRanges ((min a c, max b d) : rest) 

isFresh :: Int -> [Range] -> Bool
isFresh x ranges = any (\(l,h) -> l <= x && x <= h) ranges

main :: IO ()
main = do
  args <- getArgs
  let path = if null args then "input.txt" else head args
  content <- readFile path
  let ls = lines content
      (rangeLines, rest) = break null ls
      idLines = dropWhile null rest >>= (:[]) --
      idsPart = drop 1 rest
      ranges = map parseRange $ filter (not . null . trim) rangeLines
      ids = map read $ filter (not . null . trim) idsPart :: [Int]
      merged = mergeRanges ranges
      freshCount = length $ filter (`isFresh` merged) ids
  putStrLn $ "Fresh ingredient IDs: " ++ show freshCount
