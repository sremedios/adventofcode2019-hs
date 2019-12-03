import Data.List
import Data.Maybe

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Eq)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'                      

window :: Int -> [a] -> [[a]]
window n xs = filter (\x -> length x == n) $ transpose $ take n $ tails xs

parseDirection :: String -> Point
parseDirection s
    | direction == 'R'  = Point { x = amt, y = 0 }
    | direction == 'L'  = Point { x = -amt, y = 0 }
    | direction == 'U'  = Point { x = 0, y = amt }
    | direction == 'D'  = Point { x = 0, y = -amt }
    where direction = head s
          amt       = read $ tail s

travelPath :: [Point] -> Point -> [Point]
travelPath start next = start ++ [new_next]
    where cur_point = last start
          new_next = Point { x = (x cur_point + (x next))
                           , y = (y cur_point + (y next))
                           }
                           
accPath :: [Point] -> [Point]
accPath = flip foldl [Point { x = 0, y = 0 }] travelPath

inRange :: Int -> Int -> Int -> Bool
inRange x a b = (a <= x && x <= b) || (b <= x && x <= a)


manhattanIntersect :: (Point, Point, Point, Point) -> [Maybe Point]
manhattanIntersect (p1a, p1b, p2a, p2b)
    -- check perpindicular
    | checkRangeA = [Just Point { x = (x p1a), y = (y p2b)}]
    | checkRangeB = [Just Point { x = (x p2a), y = (y p1b)}]
    -- check colinear on x
    | x p1a == x p2a || x p1a == x p2b = [Just Point { x = (x p1a)
                                                     , y = y
                                                     }
                                         | y <- [minYP2..maxYP2]
                                         ]
    | x p1b == x p2b || x p1a == x p2b = [Just Point { x = (x p1b)
                                                     , y = y
                                                     }
                                         | y <- [minYP2..maxYP2]
                                         ]

    -- check colinear on y
    | y p1a == y p2a || y p1a == y p2b = [Just Point { x = x
                                                     , y = (y p1a)
                                                     }
                                         | x <- [minXP2..maxXP2]
                                         ]

    | y p1b == y p2b || y p1a == y p2b = [Just Point { x = x
                                                     , y = (y p1b)
                                                     }
                                         | x <- [minXP2..maxXP2]
                                         ]

    -- no intersection
    | otherwise   = [Nothing]
    where checkRangeA = inRange (x p1a) (x p2a) (x p2b) && inRange (y p2b) (y p1a) (y p1b)
          checkRangeB = inRange (x p2a) (x p1a) (x p1b) && inRange (y p1b) (y p1a) (y p1b)
          minXP2 = min (x p2a) (x p2b)
          maxXP2 = max (x p2a) (x p2b)
          minYP2 = min (y p2a) (y p2b)
          maxYP2 = max (y p2a) (y p2b)
          minXP1 = min (x p1a) (x p1b)
          maxXP1 = max (x p1a) (x p1b)
          minYP1 = min (y p1a) (y p1b)
          maxYP1 = max (y p1a) (y p1b)

shortestManhattanDist :: [Point] -> [Point] -> Int
shortestManhattanDist xs ys = foldr min (head dists) dists
    where a = window 2 xs
          b = window 2 ys
          z = zip a b
          intersections = concat $ fmap manhattanIntersect $ fmap (\([a,b],[c,d]) -> (a,b,c,d)) $ z
          legal_inters = filter(\x -> x /= Just Point{x=0,y=0} && x /= Nothing) intersections
          dists = fmap (\p -> (abs $ x p) + (abs $ y p)) $ fmap fromJust legal_inters


solveP1 :: [String] -> Int
solveP1 l = shortestManhattanDist a b
    where a = accPath $ fmap parseDirection $ wordsWhen (==',') $ head l
          b = accPath $ fmap parseDirection $ wordsWhen (==',') $ last l
             

solve :: [String] -> String
solve s = unlines (a:b:[])
    where a = "Part 1: " ++ (show $ solveP1 s)
          b = "Part 2: TODO" -- ++ (show $ solveP2 p)

main :: IO()
main = interact $ solve . lines
