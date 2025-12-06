import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    txt <- readFile "input.txt"
    let ls = lines txt
    putStrLn $ "Grand Total: " ++ show (grandTotal2 ls)

grandTotal2 :: [String] -> Integer
grandTotal2 rows =
    let padded   = pad rows
        w        = length (head padded)
        h        = length padded
        isSep c  = all (\r -> r !! c == ' ') padded

        blocks   = reverse (findBlocksRL (w-1) isSep [])

        values   = mapMaybe (evalBlock2 padded) blocks
    in sum values

pad :: [String] -> [String]
pad ls =
    let mx = maximum (map length ls)
    in [ l ++ replicate (mx - length l) ' ' | l <- ls ]

-- Find contiguous non-separator column blocks, scanning right-to-left
findBlocksRL :: Int -> (Int -> Bool) -> [(Int,Int)] -> [(Int,Int)]
findBlocksRL c isSep acc
    | c < 0     = acc
    | isSep c   = findBlocksRL (c-1) isSep acc
    | otherwise =
        let s = go c
            go k | k < 0 || isSep k = k+1
                  | otherwise       = go (k-1)
        in findBlocksRL (s-1) isSep ((s, c+1) : acc)

-- Evaluate a block: read digits column-wise
evalBlock2 :: [String] -> (Int,Int) -> Maybe Integer
evalBlock2 rows (s, e) =
    let cols = [s .. e-1]
        h    = length rows

        opRow = drop s (take e (last rows))
        op    = trim opRow

        digitsInCol c = [ rows !! r !! c | r <- [0 .. h-2], isDigit (rows !! r !! c) ]
        mkNum ds = if null ds then Nothing else parseInt ds

        nums = mapMaybe mkNum (map digitsInCol cols)
    in case (op, nums) of
        ("+", xs@(_:_)) -> Just (sum xs)
        ("*", xs@(_:_)) -> Just (product xs)
        _               -> Nothing

parseInt :: String -> Maybe Integer
parseInt s =
    case reads s of
        [(n,"")] -> Just n
        _        -> Nothing

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where dropWhileEnd p = reverse . dropWhile p . reverse
