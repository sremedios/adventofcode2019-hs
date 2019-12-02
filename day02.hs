data Program = Program { instruction_pointer  :: Int
                       , instruction :: [Int]
                       } deriving Show

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'                      

readCode :: String -> Program
readCode s = Program { instruction_pointer = 0
                     , instruction = x
                     }
    where x = map read $ wordsWhen (==',') s
    

initProgram :: Int -> Int -> Program -> Program
initProgram noun verb p = Program { instruction_pointer = instruction_pointer p
                                  , instruction = new_instruction
                                  }
    where new_instruction' = updateList 1 noun $ instruction p
          new_instruction  = updateList 2 verb new_instruction'

updateList :: Int -> Int -> [Int] -> [Int]
updateList n v l = a ++ [v] ++ b
    where (a, _:b) = splitAt n l

execute :: Program -> Program
execute p 
    | opcode == 1   = execute $ new_p (+)
    | opcode == 2   = execute $ new_p (*)
    | opcode == 99  = p
    | otherwise     = undefined
    where opcode = instruction p !! instruction_pointer p
          param_1_idx = instruction p !! (instruction_pointer p + 1)
          param_2_idx = instruction p !! (instruction_pointer p + 2)
          output_idx = instruction p !! (instruction_pointer p + 3)
          a = instruction p !! param_1_idx
          b = instruction p !! param_2_idx
          next_idx = instruction_pointer p + 4
          new_p op = Program { instruction_pointer = next_idx
                             , instruction = updateList output_idx ((op) a b) $ instruction p
                             }

solveP1 :: Program -> Int
solveP1 p = instruction p !! 0

solveP2' :: [(Int, Int)] -> Program -> [(Int, Int)]
solveP2' options p
    | result == 19690720 = [(noun, verb)]
    | otherwise          = solveP2' (tail options) p
    where result = solveP1 $ execute $ initProgram noun verb p
          noun = fst $ head options
          verb = snd $ head options

solveP2 :: Program -> Int
solveP2 p = 100 * noun + verb
    where x = solveP2' [(n, v) | n <- [0..99], v <- [0..99]] p
          noun = fst $ head x
          verb = snd $ head x
    

solve :: Program -> String
solve p = unlines (a:b:[])
    where a = "Part 1: " ++ (show $ solveP1 $ execute $ initProgram 12 2 p)
          b = "Part 2: " ++ (show $ solveP2 p)

main :: IO()
main = interact $ solve . readCode
