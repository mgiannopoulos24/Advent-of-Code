module Main where

import System.IO
import Data.List (sort)
import qualified Data.IntMap.Strict as IntMap

type Point3D = (Int, Int, Int)

type Edge = (Int, Int, Int)

type ParentMap = IntMap.IntMap Int

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let points = parseInput contents
        n = length points
    
    if n < 2 
        then putStrLn "Not enough points."
        else do
            let indexedPoints = zip [0..] points
                pointLookup = IntMap.fromList indexedPoints
                
                allEdges = [ (distSq p1 p2, i, j)
                           | (i, p1) <- indexedPoints
                           , (j, p2) <- indexedPoints
                           , i < j 
                           ]
                
                sortedEdges = sort allEdges
                
                result = solveKruskal n IntMap.empty sortedEdges pointLookup
            
            putStrLn $ "Product of X coordinates of the final connection: " ++ show result

solveKruskal :: Int -> ParentMap -> [Edge] -> IntMap.IntMap Point3D -> Int
solveKruskal components uf ((_, u, v):rest) pts
    | rootU /= rootV = 
        -- If components - 1 == 1, this union creates the single final circuit.
        if components - 1 == 1 
        then 
            let (x1, _, _) = pts IntMap.! u
                (x2, _, _) = pts IntMap.! v
            in x1 * x2
        else 
  
            solveKruskal (components - 1) (IntMap.insert rootU rootV uf) rest pts
    | otherwise = 
        solveKruskal components uf rest pts
  where
    rootU = findRoot u uf
    rootV = findRoot v uf
solveKruskal _ _ [] _ = error "Ran out of edges before forming a single circuit."

findRoot :: Int -> ParentMap -> Int
findRoot i m = case IntMap.lookup i m of
    Nothing -> i      
    Just p  -> findRoot p m

-- | Calculate squared Euclidean distance between two points
distSq :: Point3D -> Point3D -> Int
distSq (x1, y1, z1) (x2, y2, z2) = 
    (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2

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