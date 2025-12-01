module Main where

main :: IO()
main = loop 50 0 

loop :: Int -> Int -> IO()
loop curScore zerosCount = do 
    putStrLn $ "Current score/zeros is: " ++ show curScore ++ "/"++ show zerosCount
    line <- getLine
    putStrLn $ "You entered: " ++ line
    let newScore = solve line curScore
    let newZeros = if newScore == 0 then zerosCount + 1 else zerosCount
    loop newScore newZeros

-- input string like R48 or L5, current position, and returns the new position
solve :: String -> Int -> Int
solve str curPos = result
    where
        parsed = parseInstruction str
        computed = curPos + change parsed
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
