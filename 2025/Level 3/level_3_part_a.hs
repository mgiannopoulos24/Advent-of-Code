import Data.Char (isDigit, digitToInt)
import Data.List (tails)

-- Compute the maximum two-digit number achievable from ordered pairs of digits
maxTwoDigitFromLine :: String -> Int
maxTwoDigitFromLine s =
  let ds = filter isDigit s
      pairs =
        [ d1 * 10 + d2
        | (x:xs) <- tails ds
        , let d1 = digitToInt x
        , y <- xs
        , let d2 = digitToInt y
        ]
  in if null pairs then 0 else maximum pairs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let total = sum (map maxTwoDigitFromLine (lines contents))
  putStrLn $ "Total output joltages: " ++ show total