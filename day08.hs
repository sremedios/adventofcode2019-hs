import Data.List

data Layer = Layer { pixels    :: [String]
                   , num_zeros :: Int
                   , num_ones  :: Int
                   , num_twos  :: Int
                   } deriving Show

chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)    

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

toLayers :: Int -> Int -> String -> [[String]]
toLayers w h = chunks h . chunks w

parseLayer :: [String] -> Layer
parseLayer input_val = Layer { pixels = pxs
                             , num_zeros = zs
                             , num_ones = os
                             , num_twos = ts
                             }
    where pxs = input_val
          zs = count '0' $ concat pxs
          os = count '1' $ concat pxs
          ts = count '2' $ concat pxs

cmpZeros :: Layer -> Layer -> Layer
cmpZeros lhs rhs 
    | num_zeros lhs <= num_zeros rhs = lhs
    | otherwise = rhs

resolvePixel :: (Char, Char) -> Char
resolvePixel (lhs, rhs)
    | lhs == '0' || lhs  == '1' = lhs
    | otherwise                 = rhs

resolveTransparencies :: String -> String -> String
resolveTransparencies lhs rhs = map resolvePixel $ zip lhs rhs

solveP1 :: Int -> Int -> String -> Int
solveP1 w h s = num_ones fewestZeroLayer * num_twos fewestZeroLayer
    where layers = map parseLayer $ toLayers w h s
          l = head layers
          fewestZeroLayer = foldl cmpZeros l layers 

solveP2 :: Int -> Int -> String -> String
solveP2 w h s = map viz $ unlines $ head $ toLayers w h $ final_layer 
    where layers = map concat $ toLayers w h s
          l = head layers
          final_layer = foldl resolveTransparencies l layers

viz :: Char -> Char
viz c
    | c == '0' = ' '
    | c == '1' = '.'
    | otherwise = c

solve :: String -> String
solve s = unlines [a,b]
    where s' = head $ lines s
          a = "Part 1: " ++ (show $ solveP1 25 6 s')
          b = "Part 2: \n" ++ solveP2 25 6 s'

main :: IO()
main = interact $ solve
