import Data.List
import Data.Maybe
import Data.Ratio
import Data.Char
import Control.Monad

type Rat = Rational

data Machine = Machine {
    target :: [Int],    
    buttons :: [[Int]]  
} deriving Show

main :: IO ()
main = do
    content <- readFile "input.txt"
    let machines = parseInput content
    let results = map solveMachine machines
    
    putStrLn $ "Fewest button presses: " ++ show (sum results)

parseInput :: String -> [Machine]
parseInput content = 
    let ls = filter (not . null) $ lines content
    in map parseLine ls

parseLine :: String -> Machine
parseLine line = 
    let 
        (preTgt, tgtPart) = span (/= '{') line
        
        tgtStr = filter (\c -> isDigit c || c == ',') tgtPart
        tgt = if null tgtStr then [] else map read $ words $ map (\c -> if c == ',' then ' ' else c) tgtStr
        
        afterLight = dropWhile (/= ']') preTgt
        btnStr = if null afterLight then "" else drop 1 afterLight 
        
        extractGroups [] = []
        extractGroups s = 
            case dropWhile (/= '(') s of
                "" -> []
                ('(':rest) -> 
                    let (inn, rem) = span (/= ')') rest
                        nums = map read $ words $ map (\c -> if c == ',' then ' ' else c) inn
                    in nums : extractGroups rem
                    
        btnIndices = extractGroups btnStr
        
        n = length tgt
        mkVec idxs = [ if i `elem` idxs then 1 else 0 | i <- [0..n-1] ]
        btns = map mkVec btnIndices
        
    in Machine tgt btns

solveMachine :: Machine -> Integer
solveMachine (Machine tgt btns) = 
    let 
        numRows = length tgt
        numCols = length btns
        
        mat :: [[Rat]]
        mat = [ [ fromIntegral ((btns!!j)!!i) | j <- [0..numCols-1] ] ++ [ fromIntegral (tgt!!i) ] | i <- [0..numRows-1] ]
        
        (rref, pivots) = gaussian mat numCols
        
        pivotCols = map snd pivots
        freeCols = [0..numCols-1] \\ pivotCols
        
        isConsistent = all checkRow rref
        checkRow row = 
            let (coeffs, [rhs]) = splitAt numCols row
            in not (all (==0) coeffs && rhs /= 0)
            
    in if not isConsistent 
       then 0 
       else fromMaybe 0 (findMin rref pivots freeCols numCols btns tgt)

gaussian :: [[Rat]] -> Int -> ([[Rat]], [(Int, Int)])
gaussian m numVars = 
    let numRows = length m
        go r c mat pivs
            | r >= numRows || c >= numVars = (mat, pivs)
            | otherwise = 
                case findPivot r c mat of
                    Nothing -> go r (c+1) mat pivs 
                    Just pr -> 
                        let m1 = swapRows r pr mat
                            pivotVal = (m1 !! r) !! c
                            normRow = map (/ pivotVal) (m1 !! r)
                            m2 = take r m1 ++ [normRow] ++ drop (r+1) m1
                            elim i row 
                                | i == r = row
                                | otherwise = 
                                    let factor = row !! c
                                    in zipWith (\x y -> x - factor * y) row normRow
                            m3 = zipWith elim [0..] m2
                        in go (r+1) (c+1) m3 ((r,c):pivs)
                        
        findPivot r c mat = 
            let colVals = drop r $ zip mat [0..]
            in listToMaybe [ i | (row, i) <- colVals, row !! c /= 0 ]
            
        swapRows i j mat
            | i == j = mat
            | otherwise = 
                let elemI = mat !! i
                    elemJ = mat !! j
                    replace k | k == i = elemJ | k == j = elemI | otherwise = mat !! k
                in map replace [0..length mat - 1]
    in go 0 0 m []

findMin :: [[Rat]] -> [(Int, Int)] -> [Int] -> Int -> [[Int]] -> [Int] -> Maybe Integer
findMin rref pivots freeCols numVars btns tgt = 
    let 
        getBound j = 
            let aff = [ tgt!!i | i <- [0..length tgt-1], (btns!!j)!!i == 1 ]
            in if null aff then 0 else minimum aff
            
        bounds = map getBound freeCols
        
        solveRecursive [] assignedFree = 
 
            let 
                getVal c = 
                    case elemIndex c freeCols of
                        Just idx -> fromIntegral (assignedFree !! idx)
                        Nothing -> 
                            case find (\(r,p) -> p == c) pivots of
                                Nothing -> 0 
                                Just (r, _) -> 
                                    let row = rref !! r
                                        rhs = last row
                                
                                        sumFree = sum [ (row !! f) * fromIntegral (getFreeVal f) | f <- freeCols ]
                                        getFreeVal f = assignedFree !! fromJust (elemIndex f freeCols)
                                    in rhs - sumFree
                
                vals = map getVal [0..numVars-1]
                
                isValid = all (\v -> denominator v == 1 && v >= 0) vals
            in if isValid then Just (sum (map numerator vals)) else Nothing

        solveRecursive (b:bs) assigned = 
            let 
                candidates = map (\v -> solveRecursive bs (assigned ++ [v])) [0..b]
            in case catMaybes candidates of
                 [] -> Nothing
                 xs -> Just (minimum xs)
                 
    in solveRecursive bounds []