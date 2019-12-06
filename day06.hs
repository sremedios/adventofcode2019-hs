import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data FoldableOrbit = FoldableOrbit { untraversed :: [String]
                                   , traversed :: Set String
                                   , numberOrbits :: Map String Int
                                   , orbitMap :: OrbitMap
                                   } deriving Show

type OrbitMap = Map String [String]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                     "" -> []
                     s' -> w : wordsWhen p s''
                           where (w, s'') = break p s'

toTuple :: [a] -> (a, a)
toTuple l = (head l, last l)

foldableInsert :: Ord k => (k, a) -> Map k [a] -> Map k [a] 
foldableInsert input_pair m = Map.insertWith (++) (fst input_pair) ([snd input_pair]) m

readInput :: String -> FoldableOrbit
readInput s = FoldableOrbit { untraversed = ["COM"]
                            , traversed = Set.fromList []
                            , numberOrbits = Map.singleton "COM" 0
                            , orbitMap = m
                            }
    where assocs = map toTuple $ map (wordsWhen (==')')) $ lines s
          m = foldr foldableInsert (Map.fromList []) $ assocs

countOrbits' :: FoldableOrbit -> String -> FoldableOrbit
countOrbits' m s = FoldableOrbit { untraversed = next_planets
                                 , traversed = t
                                 , numberOrbits = Map.insertWith (+) s prev_count $ numberOrbits m
                                 , orbitMap = orbitMap m
                                 }
    where next_planets' = untraversed m ++ case Map.lookup s $ orbitMap m of
                                                    Just s' -> s'
                                                    Nothing -> []
          t = Set.insert s $ traversed m                         
          next_planet = head $ untraversed m
          host_planet' = filter (\(k, v) -> elem s v) $ Map.assocs $ orbitMap m 
          host_planet = case host_planet' of
                           [] -> ""
                           otherwsie -> fst $ head $ host_planet'
          prev_count = case host_planet of
                            "" -> 0
                            "COM" -> 1
                            otherwise -> 1 + case Map.lookup host_planet $ numberOrbits m of
                                    Just s' -> s'
                                    Nothing -> 0
          next_planets = filter (\x -> not $ Set.member x t) next_planets'

countOrbits :: FoldableOrbit -> FoldableOrbit
countOrbits m
    | untraversed m == [] = m
    | otherwise = countOrbits $ foldl countOrbits' m $ untraversed m


solveP1 :: String -> Int
solveP1 = sum . Map.elems . numberOrbits . countOrbits . readInput

solve :: String -> String
solve s = unlines (a:b:[])
    where a = "Part 1: " ++ (show $ solveP1 s)
          b = "Part 2: TODO"


main :: IO()
main = interact $ solve
