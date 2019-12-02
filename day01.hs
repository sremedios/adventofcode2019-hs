fuelCalc :: Int -> Int
fuelCalc x = x `div` 3 - 2

recursiveFuelCalc :: Int -> Int
recursiveFuelCalc x
    | fuel > 0   = fuel + recursiveFuelCalc fuel
    | otherwise  = 0
    where fuel = fuelCalc x



indivFuelReq :: [Int] -> [Int]
indivFuelReq = map fuelCalc

recursiveFuelReq :: [Int] -> [Int]
recursiveFuelReq = map recursiveFuelCalc

main :: IO()
--main = interact $ show . sum . indivFuelReq . map read . lines
main = interact $ show . sum . recursiveFuelReq . map read . lines
