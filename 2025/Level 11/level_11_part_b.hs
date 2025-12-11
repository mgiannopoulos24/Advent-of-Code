module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import System.IO

type Node = String
type Graph = M.Map Node [Node]
type Count = Integer

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let graph = parseInput linesOfFile

    let start = "svr"
    let end   = "out"
    let mid1  = "dac"
    let mid2  = "fft"

    let p1_part1 = countPaths graph start mid1
    let p1_part2 = countPaths graph mid1  mid2
    let p1_part3 = countPaths graph mid2  end
    let sequence1 = p1_part1 * p1_part2 * p1_part3

    let p2_part1 = countPaths graph start mid2
    let p2_part2 = countPaths graph mid2  mid1
    let p2_part3 = countPaths graph mid1  end
    let sequence2 = p2_part1 * p2_part2 * p2_part3

    let total = sequence1 + sequence2

    putStrLn $ "Paths via " ++ mid1 ++ " then " ++ mid2 ++ ": " ++ show sequence1
    putStrLn $ "Paths via " ++ mid2 ++ " then " ++ mid1 ++ ": " ++ show sequence2
    putStrLn $ "Total paths visiting both: " ++ show total

parseInput :: [String] -> Graph
parseInput lines = M.fromList [ parseLine l | l <- lines, not (null l) ]
  where
    parseLine :: String -> (Node, [Node])
    parseLine s = 
        let (src, rest) = span (/= ':') s
            targets = words (drop 1 rest) 
        in (src, targets)

countPaths :: Graph -> Node -> Node -> Count
countPaths graph start end = memo M.! start
  where
    allNodes :: [Node]
    allNodes = S.toList $ S.fromList $ M.keys graph ++ concat (M.elems graph)
    
    memo :: M.Map Node Count
    memo = M.fromList [ (u, solve u) | u <- allNodes ]
    
    solve :: Node -> Count
    solve u
        | u == end  = 1
        | otherwise = sum [ M.findWithDefault 0 v memo | v <- getNeighbors u ]
    
    getNeighbors :: Node -> [Node]
    getNeighbors u = M.findWithDefault [] u graph