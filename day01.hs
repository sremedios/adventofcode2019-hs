fuelCalc :: Int -> Int
fuelCalc x = x `div` 3 - 2

recursiveFuelCalc :: Int -> Int
recursiveFuelCalc x
    | fuel > 0   = fuel + recursiveFuelCalc fuel
    | otherwise  = 0
    where fuel = fuelCalc x

solve :: [Int] -> String
solve l = unlines (a:b:[])
    where a = show $ sum $ map fuelCalc $ l
          b = show $ sum $ map recursiveFuelCalc $ l

main :: IO()
main = interact $ show . solve . map read . lines
