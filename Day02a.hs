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

isValid :: Int -> Bool
isValid number = flag
    where
        toStr = show number
        len = length toStr
        flag = if len `mod` 2 == 1 then False else isValidR toStr 0 (len `div` 2)

isValidR :: String -> Int -> Int -> Bool
isValidR str one two
    | (str !! one == str !! two) && ((length str) - 1 == two) = True
    | (str !! one == str !! two) = isValidR str (one + 1) (two + 1)
    | otherwise = False

isInvalid :: Int -> Bool
isInvalid number = not (isValid number)
