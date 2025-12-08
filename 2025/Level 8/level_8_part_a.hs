module Main where

import System.IO
import Data.List (sort, group, sortOn)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map

type Point3D = (Int, Int, Int)

type Edge = (Int, Int, Int)

type ParentMap = Map.Map Int Int

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let points = parseInput contents
    
    let indexedPoints = zip [0..] points
    
    let allEdges = [ (distSq p1 p2, i, j)
                   | (i, p1) <- indexedPoints
                   , (j, p2) <- indexedPoints
                   , i < j -- Ensure unique pairs and no self-loops
                   ]
    
    -- Sort edges by distance (ascending) and take the top 1000
    let sortedEdges = sort allEdges
    let edgesToProcess = take 1000 sortedEdges
    
    let finalMap = foldl processEdge Map.empty edgesToProcess
    
    let allIndices = map fst indexedPoints
    let roots = map (`findRoot` finalMap) allIndices
    let circuitSizes = map length $ group $ sort roots
    
    let top3Sizes = take 3 $ sortOn Down circuitSizes
    
    putStrLn $ "Sizes of top 3 circuits: " ++ show top3Sizes
    putStrLn $ "Product of top 3 sizes: " ++ show (product top3Sizes)

parseInput :: String -> [Point3D]
parseInput raw = map parseLine $ lines raw
  where
    parseLine :: String -> Point3D
    parseLine l = 
        let parts = splitBy ',' l
            [x, y, z] = map read parts
        in (x, y, z)

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]] 
  where 
    f c l@(x:xs)
        | c == delimiter = [] : l
        | otherwise      = (c : x) : xs
    f _ [] = [] 

-- | Calculate squared Euclidean distance between two points
distSq :: Point3D -> Point3D -> Int
distSq (x1, y1, z1) (x2, y2, z2) = 
    (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2

findRoot :: Int -> ParentMap -> Int
findRoot i m = case Map.lookup i m of
    Nothing -> i      
    Just p  -> findRoot p m

union :: Int -> Int -> ParentMap -> ParentMap
union i j m =
    let rootI = findRoot i m
        rootJ = findRoot j m
    in if rootI /= rootJ
       then Map.insert rootI rootJ m 
       else m

processEdge :: ParentMap -> Edge -> ParentMap
processEdge m (_, u, v) = union u v m