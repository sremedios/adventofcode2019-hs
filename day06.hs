import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data FoldableOrbit = FoldableOrbit { untraversedPlanets :: [String]
                                   , traversedPlanets :: Set String
                                   , numberOrbits :: Int
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

parseInputString :: String -> [(String, String)]
parseInputString = map toTuple . map (wordsWhen (==')')) . lines

-- first pass through input
parsePairs :: String -> FoldableOrbit
parsePairs s = FoldableOrbit { untraversedPlanets = ["COM"]
                             , traversedPlanets = Set.fromList []
                             , numberOrbits = 0
                             , orbitMap = m
                             }
    where m = foldr foldableInsert (Map.fromList []) $ parseInputString s

countOrbits' :: FoldableOrbit -> String -> FoldableOrbit
countOrbits' m s = FoldableOrbit { untraversedPlanets = next_planets'
                                , traversedPlanets = Set.insert s $ t
                                , numberOrbits = (numberOrbits m) + (length next_planets)
                                , orbitMap = orbitMap m
                                }
    where next_planets' = case Map.lookup s $ orbitMap m of
                                Just s' -> s'
                                Nothing -> []
          t = traversedPlanets m
          next_planets'' = next_planets' ++ untraversedPlanets m
          next_planets = filter (\x -> not (Set.member x t)) next_planets''

countOrbits :: FoldableOrbit -> FoldableOrbit
countOrbits m
    | untraversedPlanets m == [] = m
    | otherwise = countOrbits $ foldl countOrbits' m $ untraversedPlanets m


solveP1 :: String -> Int
solveP1 = undefined -- length . concat . Map.elems . updateOrbit . parsePairs

solve :: String -> String
solve s = unlines (a:b:[])
    where a = "Part 1: " ++ (show $ solveP1 s)
          b = "Part 2: TODO"


main :: IO()
main = interact $ solve
