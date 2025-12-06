import Data.Char (isSpace)
import Data.List (transpose)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    txt <- readFile "input.txt"
    let ls = lines txt
    putStrLn $ "Grand Total: " ++ show (grandTotal ls)

-- Compute grand total from raw rows
grandTotal :: [String] -> Integer
grandTotal rows =
    let padded = pad rows
        w      = length (head padded)
        isSep c = all (\r -> r !! c == ' ') padded
        blocks  = findBlocks 0 w isSep []
        slices  = map (\(s,e) -> map (trim . take (e-s) . drop s) padded) blocks
        values  = mapMaybe evalBlock slices
    in sum values

-- Pad all rows to same width
pad :: [String] -> [String]
pad ls =
    let mx = maximum (map length ls)
    in [ l ++ replicate (mx - length l) ' ' | l <- ls ]

-- Find contiguous non-separator column blocks
findBlocks :: Int -> Int -> (Int -> Bool) -> [(Int,Int)] -> [(Int,Int)]
findBlocks i w isSep acc
    | i >= w    = reverse acc
    | isSep i   = findBlocks (i+1) w isSep acc
    | otherwise =
        let j = until (\k -> k>=w || isSep k) (+1) (i+1)
        in findBlocks j w isSep ((i,j):acc)

-- Evaluate a single block (numbers vertically, op on last row)
evalBlock :: [String] -> Maybe Integer
evalBlock [] = Nothing
evalBlock rs =
    let (numsRows, [opRow]) = splitAt (length rs - 1) rs
        op   = trim opRow
        nums = mapMaybe parseInt (filter (not . null) (map trim numsRows))
    in case (op, nums) of
         ("+", xs@(_:_)) -> Just (sum xs)
         ("*", xs@(_:_)) -> Just (product xs)
         _               -> Nothing

-- Parse an integer safely
parseInt :: String -> Maybe Integer
parseInt s =
    case reads s of
      [(n, rest)] | all isSpace rest -> Just n
      _ -> Nothing

-- Trim spaces
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where dropWhileEnd p = reverse . dropWhile p . reverse
