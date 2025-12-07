import System.IO
import Data.Array
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
        rows = length ls
        cols = length (head ls)

        gridArray = listArray ((0,0),(rows-1,cols-1))
                        (concat ls)

        -- locate S
        start = head [ (r,c)
                     | r <- [0..rows-1]
                     , c <- [0..cols-1]
                     , gridArray ! (r,c) == 'S'
                     ]

        (sr, sc) = start
        initialQueue = Seq.singleton (sr+1, sc)
        visited = Set.empty

    putStrLn $ "The beam be split " ++show (simulate gridArray rows cols initialQueue visited 0) ++ " times."

simulate
  :: Array (Int,Int) Char
  -> Int -> Int
  -> Seq (Int,Int)
  -> Set.Set (Int,Int)
  -> Int
  -> Int
simulate _ _ _ Seq.Empty _ !acc = acc
simulate grid rows cols ((r,c) :<| qs) seen !acc
    | r < 0 || r >= rows || c < 0 || c >= cols =
        simulate grid rows cols qs seen acc
    | (r,c) `Set.member` seen =
        simulate grid rows cols qs seen acc
    | otherwise =
        case grid ! (r,c) of
            '^' ->
                let newSeen = Set.insert (r,c) seen
                    qs' = qs |> (r,c-1) |> (r,c+1)
                in simulate grid rows cols qs' newSeen (acc+1)

            _ ->
                let newSeen = Set.insert (r,c) seen
                    qs' = qs |> (r+1,c)
                in simulate grid rows cols qs' newSeen acc
