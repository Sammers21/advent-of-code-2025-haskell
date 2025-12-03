module Main where

import Data.List (sortBy)
import Data.Ord (Down(..), comparing)

main :: IO ()
main = compute 0

compute :: Int -> IO()
compute currentSum = do 
    putStrLn $ "Current sum is " ++ show currentSum
    line <- getLine
    let lineVolts = computeVolts line
    compute (currentSum + lineVolts)

-- | Represents a digit found in the input string along with its original index.
data DigitAndIndex = DigitAndIndex {
    digit :: Int, -- ^ The actual numeric value (0-9)
    index :: Int  -- ^ The position in the source string
} deriving (Show, Eq, Ord)

-- | Calculates the "volts" from a raw input string by extracting specific digits.
computeVolts :: String -> Int
computeVolts str = res
    where 
        allDigits = map (\(char, idx) ->  DigitAndIndex { digit = read [char] :: Int, index = idx }) (zip str [0..])
        digits = digitArr allDigits 12
        res = glue digits

-- | Glue the digits back together into a single Int
glue :: [DigitAndIndex] -> Int
glue ints = read (foldl (++) "" (map (\x -> show (digit x)) ints)) :: Int

-- Find N biggest digits(each digit as big as possilbe with ) that are not in the end
--          currentDigits       count  accumulator        result
digitArr :: [DigitAndIndex] -> Int -> [DigitAndIndex] 
digitArr currentDigits count
    | count == 0 = []
    | otherwise = res
    where
        biggest = findOneBiggestWithLowestIdx (dropLast (count-1) currentDigits)
        newCurrentDigits = filter (\x -> index x /= index biggest) currentDigits
        newCurrentDigitsTwo = filter (\x -> index x > index biggest) newCurrentDigits
        res = [biggest] ++ (digitArr newCurrentDigitsTwo (count - 1)) 

findOneBiggestWithLowestIdx :: [DigitAndIndex] -> DigitAndIndex
findOneBiggestWithLowestIdx digits = biggest
    where
        sorted = sortBy (\a b -> 
            case compare (digit b) (digit a) of -- Compare digits desc (b vs a)
                EQ -> compare (index a) (index b) -- If digits equal, compare index asc (a vs b)
                ord -> ord) digits
        biggest = head sorted

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs

