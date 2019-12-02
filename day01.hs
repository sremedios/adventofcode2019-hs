fuelCalc :: Int -> Int
fuelCalc x = x `div` 3 - 2

recursiveFuelCalc :: Int -> Int
recursiveFuelCalc x
    | fuel > 0   = fuel + recursiveFuelCalc fuel
    | otherwise  = 0
    where fuel = fuelCalc x

main :: IO()
--main = interact $ show . sum . map fuelCalc . map read . lines
main = interact $ show . sum . map recursiveFuelCalc . map read . lines
