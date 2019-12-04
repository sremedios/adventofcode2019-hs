import Data.List
import Data.Maybe

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Eq)

data Seg = Seg { ps   :: [Point]
               , lens :: [Int]
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

travelPath :: Seg -> Point -> Seg
travelPath start next = new_seg
    where cur_point = last $ ps start
          new_next = Point { x = (x cur_point + (x next))
                           , y = (y cur_point + (y next))
                           }
          new_seg = Seg { ps = ps start ++ [new_next]
                        , lens = lens start ++ [(last (lens start) + (abs (x next) + abs (y next)))]
                        }
                           
accPath :: [Point] -> Seg
accPath = flip foldl Seg {ps=[Point { x = 0, y = 0 }], lens=[0]} travelPath

inRange :: Int -> Int -> Int -> Bool
inRange x a b = (a <= x && x <= b) || (b <= x && x <= a)


manhattanIntersect :: (Point, Point, Point, Point) -> Maybe Point
manhattanIntersect (p1a, p1b, p2a, p2b)
    -- check perpindicular
    | checkRangeA = Just Point { x = (x p1a), y = (y p2b)}
    | checkRangeB = Just Point { x = (x p2a), y = (y p1b)}
    -- no intersection
    | otherwise   = Nothing
    where checkRangeA = inRange (x p1a) (x p2a) (x p2b) && inRange (y p2b) (y p1a) (y p1b)
          checkRangeB = inRange (x p2a) (x p1a) (x p1b) && inRange (y p1b) (y p2a) (y p2b)

shortestManhattanDist :: Seg -> Seg -> Int
shortestManhattanDist x_seg y_seg = foldr min (head dists) dists
    where xs = ps x_seg
          ys = ps y_seg
          z = [(a, b) | a <- window 2 xs, b <- window 2 ys]
          intersections = fmap manhattanIntersect $ fmap (\([a,b],[c,d]) -> (a,b,c,d)) z
          legal_inters = filter(\x -> x /= Just Point{x=0,y=0} && x /= Nothing) intersections
          dists = fmap (\p -> (abs $ x p) + (abs $ y p)) $ fmap fromJust legal_inters

shortestSteps :: Seg -> Seg -> Int
shortestSteps x_seg y_seg = foldr min (head steps) steps
    where xs = ps x_seg
          ys = ps y_seg
          z = [(a, b) | a <- window 2 xs, b <- window 2 ys]
          intersections = fmap manhattanIntersect $ fmap (\([a,b],[c,d]) -> (a,b,c,d)) z
          legal_inters = filter(\x -> x /= Just Point{x=0,y=0} && x /= Nothing) intersections
          x_lens = fmap lens $ fmap (\x -> travelPath x_seg x) $ fmap fromJust legal_inters
          y_lens = fmap lens $ fmap (\y -> travelPath y_seg y) $ fmap fromJust legal_inters
          steps = fmap (\pair -> fst pair + snd pair) $ zip (last x_lens) (last y_lens)

solveP1 :: [String] -> Int
solveP1 l = shortestManhattanDist a b
    where a = accPath $ fmap parseDirection $ wordsWhen (==',') $ head l
          b = accPath $ fmap parseDirection $ wordsWhen (==',') $ last l

solveP2 :: [String] -> Int
solveP2 l = shortestSteps a b
    where a = accPath $ fmap parseDirection $ wordsWhen (==',') $ head l
          b = accPath $ fmap parseDirection $ wordsWhen (==',') $ last l

test :: [String] -> [Seg ]
test l = test' a b
    where a = accPath $ fmap parseDirection $ wordsWhen (==',') $ head l
          b = accPath $ fmap parseDirection $ wordsWhen (==',') $ last l

test' :: Seg -> Seg -> [Seg]
test' x_seg y_seg = t
    where xs = ps x_seg
          ys = ps y_seg
          z = [(a, b) | a <- window 2 xs, b <- window 2 ys]
          intersections = fmap manhattanIntersect $ fmap (\([a,b],[c,d]) -> (a,b,c,d)) z
          legal_inters = filter(\x -> x /= Just Point{x=0,y=0} && x /= Nothing) intersections
          x_lens = fmap lens $ fmap (\x -> travelPath x_seg x) $ fmap fromJust legal_inters
          t = fmap (\x -> travelPath x_seg x) $ fmap fromJust legal_inters
             

solve :: [String] -> String
solve s = unlines (a:b:[])
    where a = "Part 1: " ++ (show $ solveP1 s)
          b = "Part 2: TODO" -- ++ (show $ solveP2 s)

main :: IO()
main = interact $ solve . lines
