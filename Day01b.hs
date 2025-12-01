module Main where

main :: IO()
main = loop 50 0 

loop :: Int -> Int -> IO()
loop curScore zerosCount = do 
    putStrLn $ "Current score/zeros is: " ++ show curScore ++ "/"++ show zerosCount
    line <- getLine
    let (newScore, zerosTurns) = solve line curScore
    let newZeros =  zerosTurns + (if newScore == 0 then zerosCount + 1 else zerosCount)
    loop newScore newZeros

-- input string like R48 or L5, current position, and returns the new position and zeros turns
solve :: String -> Int -> (Int, Int)
solve str curPos = (result, zerosTurns)
    where
        parsed = parseInstruction str
        diff = change parsed
        diffMod100 = (abs diff `mod` 100) 
        diffMod100WSign = (diffMod100 * (if diff < 0 then -1 else 1))
        computed = curPos + diffMod100WSign
        zerosTurns = (abs diff `div` 100) + (if (curPos /= 0 && (computed < 0 || computed > 100)) then 1 else 0)
        result = computed `mod` 100

change :: (Char, Int) -> Int
change (direction, distance)
    | direction == 'R' = distance
    | otherwise        = -distance

parseInstruction :: String -> (Char, Int)
parseInstruction s = (direction, distance)
    where
        direction = head s :: Char
        distance = read (tail s) :: Int
