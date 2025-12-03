import Data.Char (isDigit, digitToInt)

-- Pick exactly k digits in order, maximizing the lexicographic result.
maxSubseq :: Int -> [Int] -> [Int]
maxSubseq k xs = go k xs
  where
    go 0 _  = []
    go _ [] = []  -- shouldn't happen with valid input
    go n ys =
      let takeLimit = length ys - n
          (searchRange, rest) = splitAt (takeLimit + 1) ys
          best = maximum searchRange
          -- drop until we find best (first occurrence)
          (_:afterBest) = dropWhile (/= best) ys
      in best : go (n - 1) afterBest

-- For each line, extract digits and pick the best 12-digit subsequence.
max12FromLine :: String -> Integer
max12FromLine s =
  let ds = map digitToInt (filter isDigit s)
      picked = maxSubseq 12 ds
  in read (concatMap show picked)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let total :: Integer
      total = sum (map max12FromLine (lines contents))
  putStrLn $ "New total output joltages: " ++ show total