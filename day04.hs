import Control.Monad
import Data.List

window :: Int -> [a] -> [[a]]
window n xs = filter (\x -> length x == n) $ transpose $ take n $ tails xs

adj :: String -> Bool
adj = any (\s -> head s == last s) . window 2

adjTwo :: String -> Bool
adjTwo s = any (==True) $ fmap adj $ filter (\(a:b:c:d:_) -> a/=b&&b==c&&c/=d) $ window 4 (s ++ "  ")

alwaysInc :: String -> Bool
alwaysInc = all (\s -> head s <= last s) . window 2

solveP1 :: Int -> Int -> Int
solveP1 start end = length $ filter (liftM2 (&&) adj alwaysInc . show) [start..end]

solveP2 :: Int -> Int -> Int
solveP2 start end = length $ filter (liftM2 (&&) adjTwo alwaysInc . show) [start..end]

solve :: [Int] -> String
solve ranges = unlines (a:b:[])
    where a = "Part 1: " ++ show (solveP1 (head ranges) (last ranges))
          b = "part 2: " ++ show (solveP2 (head ranges) (last ranges))

main :: IO()
main = interact $ solve . map read . words 
