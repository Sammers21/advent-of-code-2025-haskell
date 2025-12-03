module Main where

main :: IO ()
main = compute 0

compute :: Int -> IO ()
compute s = do
    putStrLn $ "Current sum is " ++ show s
    l <- getLine
    compute (s + read (solve l 12))

solve :: String -> Int -> String
solve _ 0 = []
solve s n = m : solve (drop (length (takeWhile (/= m) c) + 1) s) (n - 1)
    where
        c = take (length s - n + 1) s
        m = maximum c
