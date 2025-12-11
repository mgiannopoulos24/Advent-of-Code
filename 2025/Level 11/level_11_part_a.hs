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
    
    let result = countPaths graph "you" "out"
    
    putStrLn $ "Total paths from 'you' to 'out': " ++ show result

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
        | otherwise = sum [ memo M.! v | v <- getNeighbors u ]
    
    getNeighbors :: Node -> [Node]
    getNeighbors u = M.findWithDefault [] u graph