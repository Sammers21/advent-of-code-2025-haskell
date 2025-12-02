module Main where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter str = case break (== delimiter) str of
    (before, []) -> [before]
    (before, _:after) -> before : splitOn delimiter after

main :: IO ()
main = do
    putStrLn "You can input your data"
    line <- getLine
    let split = splitOn ',' line
    let numbers = concat [ [start..end] | (start, end) <- map parseRange split ]
    let total = foldl (+) 0 (filter isValid numbers)
    putStrLn ("Sum of invalid is: " ++ show total)

parseRange :: String -> (Int, Int)
parseRange str = (from, to)
    where
        dashSplit = splitOn '-' str
        from = read (head dashSplit) :: Int
        to = read (dashSplit !! 1) :: Int

isInvalid :: Int -> Bool
isInvalid number = not (isValid number)


isValid :: Int -> Bool
isValid number = isValidR (show number) 1

-- string itself -> sequence number
isValidR :: String -> Int -> Bool
isValidR numStr sequenceLength
    | (((length numStr) `div` sequenceLength) == 0) = False
    | (((length numStr) `mod` sequenceLength) /= 0) = isValidR numStr (sequenceLength + 1)
    | otherwise = if isValidCaseR numStr sequenceLength 0 then True else isValidR numStr (sequenceLength + 1)

-- string itself -> seqLen -> idx 
isValidCaseR :: String -> Int -> Int -> Bool
isValidCaseR numStr sequenceLength idx 
    | groupsCount < 2 = False
    | idx >= length numStr = True
    | idx * groupsCount >= length numStr = True
    | otherwise = result
    where
        groupsCount = (length numStr) `div` sequenceLength
        first = numStr !! idx
        indices = map (\i -> idx + i * sequenceLength) [0..groupsCount-1]
        validIndices = filter (< length numStr) indices
        matches = all (\i -> numStr !! i == first) validIndices
        result = if matches
                 then isValidCaseR numStr sequenceLength (idx + 1)
                 else False
    