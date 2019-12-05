data Program = Program { instruction_pointer  :: Int
                       , memory               :: [Int]
                       , current_input        :: Int
                       , current_output       :: Int
                       } deriving Show

data Mode = Immediate | Position | Write deriving Show                       

data Operation = Add | Mul | Inp | Out | Halt deriving Show

data Instruction = Instruction { op :: Operation
                               , param_1 :: Mode 
                               , param_2 :: Mode
                               , param_3 :: Mode
                               , num_params :: Int
                               } deriving Show

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'                      

updateList :: Int -> Int -> [Int] -> [Int]
updateList n v l = a ++ [v] ++ b
    where (a, _:b) = splitAt n l

readCode :: Int -> String -> Program
readCode input_value s = Program { instruction_pointer = 0
                                 , memory = map read $ wordsWhen (==',') s
                                 , current_input = input_value
                                 , current_output = 0 
                                 }

parseOpcode :: Int -> String
parseOpcode n = num_zeros ++ s
    where s = show n
          l = length s
          num_zeros = replicate (5-l) '0'

parseMode :: Char -> Mode 
parseMode c
    | c == '0'  = Position
    | c == '1'  = Immediate
    | otherwise = undefined

parseInstruction :: String -> Instruction
parseInstruction opcode
    | in_instruct == 1  = out_instruct Add 4
    | in_instruct == 2  = out_instruct Mul 4
    | in_instruct == 3  = out_instruct Inp 2
    | in_instruct == 4  = out_instruct Out 2
    | in_instruct == 99 = out_instruct Halt 0
    | otherwise         = undefined
    where in_instruct                   = read $ drop 3 opcode
          param_1_mode                  = parseMode $ opcode !! 2
          param_2_mode                  = parseMode $ opcode !! 1
          param_3_mode                  = Write
          out_instruct op num_params    = Instruction { op          = op
                                                      , param_1     = param_1_mode
                                                      , param_2     = param_2_mode
                                                      , param_3     = param_3_mode
                                                      , num_params  = num_params
                                                      }

parseArg :: Program -> Int -> Mode -> Int
parseArg p arg_num m = case m of
                    Position -> mem !! (mem !! cur_idx)
                    Immediate -> mem !! cur_idx
                    Write -> mem !! cur_idx
    where instr_ptr = instruction_pointer p
          cur_idx = arg_num + instr_ptr
          mem = memory p


executeInstruction :: Program -> Program 
executeInstruction p = Program { instruction_pointer = cur_idx + num_params instr
                                , memory = new_memory
                                , current_input = current_input p
                                , current_output = memory p !! arg_1
                                }

    where cur_idx = instruction_pointer p
          opcode = parseOpcode $ memory p !! cur_idx
          instr = parseInstruction $ opcode
          arg_1 = parseArg p 1 $ param_1 instr
          arg_2 = parseArg p 2 $ param_2 instr
          arg_3 = parseArg p 3 $ param_3 instr
          new_memory = case op instr of
                            Add  -> updateList arg_3 (arg_1 + arg_2) $ memory p
                            Mul  -> updateList arg_3 (arg_1 * arg_2) $ memory p
                            Inp  -> updateList arg_1 (current_input p) $ memory p
                            Out  -> memory p
                            Halt -> memory p



{-
execute :: Int -> Program -> Program
execute user_input p 
    | opcode == 1   = execute user_input $ new_p 2 $ arithmetic_instruction (+)
    | opcode == 2   = execute user_input $ new_p 2 $ arithmetic_instruction (*)
    | opcode == 3   = execute user_input $ new_p 1 $ input_instruction
    | opcode == 4   = execute user_input $ new_p 1 $ memory p
    | opcode == 99  = p
    | otherwise     = undefined
    where opcode = memory p !! instruction_pointer p
          param_1_idx = memory p !! (instruction_pointer p + 1)
          param_2_idx = memory p !! (instruction_pointer p + 2)
          param_3_idx = memory p !! (instruction_pointer p + 3)
          a = memory p !! param_1_idx
          b = memory p !! param_2_idx
          next_idx num_params = instruction_pointer p + num_params
          arithmetic_instruction op = updateList param_3_idx ((op) a b) $ memory p
          input_instruction = updateList param_1_idx user_input $ memory p
          new_p num_params memory = Program { instruction_pointer = next_idx num_params
                                            , memory = memory 
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

-}
main :: IO()
main = undefined -- interact $ solve . readCode
