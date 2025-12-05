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

-- Merge overlapping or adjacent ranges.
mergeRanges :: [Range] -> [Range]
mergeRanges rs = foldr insertSorted [] (sortBy (comparing fst) rs)
  where
    insertSorted r [] = [r]
    insertSorted (a,b) acc@((c,d):rest)
      | b < c - 1  = (a,b) : acc
      | a > d + 1  = (a,b) : acc
      | otherwise  = mergeRanges ((min a c, max b d) : rest)

main :: IO ()
main = do
  args <- getArgs
  let path = if null args then "input.txt" else head args
  content <- readFile path
  let ls = lines content
      (rangeLines, _) = break null ls
      ranges = map parseRange $ filter (not . null . trim) rangeLines
      merged = mergeRanges ranges

      totalFresh = sum [ hi - lo + 1 | (lo,hi) <- merged ]

  putStrLn $ "Fresh ingredient IDs: " ++ show totalFresh
