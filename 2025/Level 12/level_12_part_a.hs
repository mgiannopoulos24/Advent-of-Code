import System.IO
import Data.List (sort, sortOn, nub, intercalate, isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Char (isDigit, digitToInt)
import Control.Monad (forM_)

type Coord = (Int, Int) 
type Shape = [Coord]    
type Grid = S.Set Coord 

data Query = Query 
    { qDims :: Coord       
    , qPieces :: [Int]     
    } deriving (Show)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfFile = filter (not . null) $ lines content
    let (shapeMap, queries) = parseInput linesOfFile
    
    let processedShapes = M.map processShape shapeMap

    let solvableCount = length $ filter (solveQuery processedShapes) queries
    
    putStrLn $ "Number of regions that can fit their presents: " ++ show solvableCount

parseInput :: [String] -> (M.Map Int Shape, [Query])
parseInput [] = (M.empty, [])
parseInput lines = (M.fromList shapes, queries)
  where

    isQueryLine l = let (w, rest) = span isDigit l in not (null w) && not (null rest) && head rest == 'x'
    
    (shapeLines, queryLines) = span (not . isQueryLine) lines
    
    shapes = parseShapes shapeLines
    queries = map parseQuery queryLines

parseShapes :: [String] -> [(Int, Shape)]
parseShapes [] = []
parseShapes (header:rest)
    | ':' `elem` header = 
        let idStr = takeWhile isDigit header
            sId = read idStr :: Int
            (body, remaining) = span (\l -> not (':' `elem` l) || (':' `elem` l && not (isDigit (head l)))) rest
            coords = parseGrid body
        in (sId, coords) : parseShapes remaining
    | otherwise = parseShapes rest 

parseGrid :: [String] -> Shape
parseGrid rows = 
    [ (r, c) | (r, row) <- zip [0..] rows, (c, char) <- zip [0..] row, char == '#' ]

parseQuery :: String -> Query
parseQuery line = 
    let (dimPart, rest) = span (/= ':') line
        countsPart = drop 1 rest
        [wStr, hStr] = words $ map (\c -> if c == 'x' then ' ' else c) dimPart
        width = read wStr
        height = read hStr
        counts = map read $ words countsPart
       
        pieceList = concat $ zipWith replicate counts [0..]
    in Query (width, height) pieceList

processShape :: Shape -> (Int, [Shape])
processShape s = (length s, uniqueVars)
  where
    uniqueVars = nub $ map normalize $ transformations s

    normalize :: Shape -> Shape
    normalize coords = sort [ (r - minR, c - minC) | (r, c) <- coords ]
      where
        minR = minimum $ map fst coords
        minC = minimum $ map snd coords

    transformations :: Shape -> [Shape]
    transformations base = 
        let r0 = base
            r1 = rotate r0
            r2 = rotate r1
            r3 = rotate r2
            flips = map flipShape [r0, r1, r2, r3]
        in [r0, r1, r2, r3] ++ flips

    rotate :: Shape -> Shape
    rotate = map (\(r, c) -> (c, -r))
    
    flipShape :: Shape -> Shape
    flipShape = map (\(r, c) -> (r, -c))

solveQuery :: M.Map Int (Int, [Shape]) -> Query -> Bool
solveQuery shapeData (Query (w, h) pieces) = 
    let 
     
        piecesWithData = map (\pid -> (pid, shapeData M.! pid)) pieces
        sortedPieces = sortOn (\(_, (area, _)) -> negate area) piecesWithData
        
        totalArea = w * h
        reqArea = sum $ map (\(_, (a,_)) -> a) sortedPieces
    in
        if reqArea > totalArea then False 
        else canFit (w, h) S.empty sortedPieces (-1) (-1)

canFit :: (Int, Int) -> Grid -> [(Int, (Int, [Shape]))] -> Int -> Int -> Bool
canFit _ _ [] _ _ = True -- All pieces placed
canFit (w, h) occupied ((pid, (area, vars)) : remaining) prevId minIdx = 
    let 
        
        
      
        startIndex = if pid == prevId then minIdx else 0
        
      
    in any attemptPlacement [ (var, r, c) | 
                              var <- vars,
                              r <- [0..h-1], 
                              c <- [0..w-1],
                              let linIdx = r * w + c,
                              linIdx >= startIndex
                            ]
  where
    attemptPlacement (shape, r, c) =
        let 
            
            shifted = map (\(dr, dc) -> (r + dr, c + dc)) shape
            
            inBounds = all (\(br, bc) -> br >= 0 && br < h && bc >= 0 && bc < w) shifted
            noOverlap = all (`S.notMember` occupied) shifted
        in 
            if inBounds && noOverlap
            then 
                let newOccupied = S.union occupied (S.fromList shifted)
                    
                    currentLinIdx = r * w + c
                in canFit (w, h) newOccupied remaining pid currentLinIdx
            else False
