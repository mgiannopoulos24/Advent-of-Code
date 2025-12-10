import Data.List
import Data.Maybe
import Data.Char
import Data.Bits
import Control.Monad

type Vector = [Int]
type Matrix = [Vector]

data Machine = Machine {
    target :: Vector,   
    buttons :: [Vector] 
} deriving Show

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesList = filter (not . null) $ lines content
        machines = map parseLine linesList
        results = map solveMachine machines
        total = sum results

    putStrLn $ "Fewest button presses: " ++ show total

parseLine :: String -> Machine
parseLine line = 
    let (part1, rest1) = span (/= ']') line
        lightStr = dropWhile (not . (`elem` ".#")) part1
        tgt = map (\c -> if c == '#' then 1 else 0) lightStr
        n = length tgt
        
        findGroups [] = []
        findGroups s = 
            case dropWhile (/= '(') s of
                "" -> []
                ('(':rest) -> 
                    let (content, remainder) = span (/= ')') rest
                        cleanContent = map (\c -> if c == ',' then ' ' else c) content
                        nums = map read $ words cleanContent
                    in nums : findGroups remainder
        
        btnIndices = findGroups rest1
        
        mkBtnVec indices = 
            let vec = replicate n 0
                setOne i v = take i v ++ [1] ++ drop (i+1) v
            in foldl (\v idx -> setOne idx v) vec indices
        
        btns = map mkBtnVec btnIndices
    in Machine tgt btns

solveMachine :: Machine -> Int
solveMachine (Machine tgt btns) = 
    let numRows = length tgt
        numCols = length btns
        
        -- Construct the matrix for Ax = T
        buildRow i = [ (btns !! j) !! i | j <- [0..numCols-1] ] ++ [tgt !! i]
        matrix = [ buildRow i | i <- [0..numRows-1] ]
        
        (rref, pivots) = gaussianElimination matrix numCols
        
        pivotCols = map snd pivots
        freeCols = [0..numCols-1] \\ pivotCols
        
        isConsistent = all checkRow rref
        checkRow row = 
            let (coeffs, [rhs]) = splitAt numCols row
            in not (all (==0) coeffs && rhs == 1)
            
    in if not isConsistent 
       then error "Machine configuration is impossible!" 
       else findMinPresses rref pivots freeCols numCols

gaussianElimination :: Matrix -> Int -> (Matrix, [(Int, Int)])
gaussianElimination m numVars = 
    let numRows = length m
        go row col matrix pivs
            | row >= numRows || col >= numVars = (matrix, pivs)
            | otherwise = 
                case findPivot row col matrix of
                    Nothing -> go row (col + 1) matrix pivs 
                    Just pRow -> 
                        let m1 = swapRows row pRow matrix
                            pivotRowVal = m1 !! row
                            m2 = map (\(r, i) -> if i /= row && (r !! col) == 1 
                                                 then zipWith xor r pivotRowVal 
                                                 else r) (zip m1 [0..])
                        in go (row + 1) (col + 1) m2 ((row, col) : pivs)
        
        findPivot r c mat = 
            let candidates = drop r $ zip mat [0..]
            in listToMaybe [ i | (rowVec, i) <- candidates, rowVec !! c == 1 ]
            
        swapRows i j mat 
            | i == j = mat
            | otherwise = 
                let elemI = mat !! i
                    elemJ = mat !! j
                    replace k
                        | k == i = elemJ
                        | k == j = elemI
                        | otherwise = mat !! k
                in map replace [0..length mat - 1]
                
    in go 0 0 m []

findMinPresses :: Matrix -> [(Int, Int)] -> [Int] -> Int -> Int
findMinPresses rref pivots freeCols numVars = 
    let 
        colToRow = map (\(r, c) -> (c, r)) pivots
        
        -- Generate all combinations for free variables (0 or 1)
        assignments = sequence (replicate (length freeCols) [0, 1])
        
        solveFor freeVals = 
            let 
                getFreeVal c = 
                     let idx = fromJust (elemIndex c freeCols)
                     in freeVals !! idx
                
                getVal c = 
                    case lookup c colToRow of
                        Nothing -> getFreeVal c 
                        Just r -> 
                            let rowVec = rref !! r
                                rhs = last rowVec
                                sumFree = foldl' (\acc f -> 
                                    if (rowVec !! f) == 1 
                                    then acc `xor` getFreeVal f 
                                    else acc) 0 freeCols
                            in rhs `xor` sumFree
                
                solution = map getVal [0..numVars-1]
            in sum solution
            
    in minimum (map solveFor assignments)