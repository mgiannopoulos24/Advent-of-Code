import System.IO
import Data.Array

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
        rows = length ls
        cols = length (head ls)

        grid = listArray ((0,0), (rows-1, cols-1)) (concat ls)

        start = head [ (r,c)
                     | r <- [0..rows-1]
                     , c <- [0..cols-1]
                     , grid ! (r,c) == 'S'
                     ]

        (sr, sc) = start

        -- memoization array: stores the number of timelines resulting from a particle at (r,c)
        memo :: Array (Int,Int) Integer
        memo = listArray ((0,0), (rows-1, cols-1))
                 [ solve r c | r <- [0..rows-1], c <- [0..cols-1] ]

        solve r c =
            case grid ! (r,c) of
                -- Splitter: Timeline splits into two (Left and Right)
                '^' -> get (r, c-1) + get (r, c+1)
                _   -> get (r+1, c)

        get (r,c)
            | r < 0 || r >= rows = 1
            | c < 0 || c >= cols = 1
            | otherwise          = memo ! (r,c)

    putStrLn $ "Total timelines: " ++ show (get (sr+1, sc))