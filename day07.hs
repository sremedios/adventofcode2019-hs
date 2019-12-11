import Data.List

data Program = Program { instruction_pointer  :: Int
                       , memory               :: [Int]
                       , prev_op              :: Operation
                       , phase_setting        :: [Int]
                       , phase_setting_ptr    :: Int
                       , output_value       :: Int
                       } deriving Show

data Mode = Immediate | Position | Write deriving Show                       

data Operation = Add 
               | Mul 
               | Inp 
               | Out 
               | Halt 
               | Start 
               | JumpIfTrue
               | JumpIfFalse
               | LessThan
               | Equals
               deriving Show

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

readCode :: String -> Program
readCode s = Program { instruction_pointer = 0
                     , memory = map read $ wordsWhen (==',') s
                     , phase_setting = []
                     , phase_setting_ptr = 0
                     , prev_op = Start
                     , output_value = 0 
                     }

setInput :: Int -> Program -> Program
setInput input_value p = Program { instruction_pointer = instruction_pointer p
                                 , memory = memory p
                                 , phase_setting = phase_setting p
                                 , phase_setting_ptr = 0
                                 , prev_op = prev_op p
                                 , output_value = output_value p
                                 }

copyMem :: [Int] -> Program -> Program
copyMem mem p = Program { instruction_pointer = instruction_pointer p
                              , memory = mem
                              , phase_setting = phase_setting p
                              , phase_setting_ptr = phase_setting_ptr p
                              , prev_op = prev_op p
                              , output_value = output_value p
                              }

setPhase :: [Int] -> Program -> Program
setPhase settings p = Program { instruction_pointer = instruction_pointer p
                              , memory = memory p
                              , phase_setting = settings
                              , phase_setting_ptr = 0
                              , prev_op = prev_op p
                              , output_value = output_value p
                              }

updatePhase :: Program -> Program
updatePhase p = Program { instruction_pointer = instruction_pointer p
                        , memory = memory p
                        , phase_setting = phase_setting p
                        , phase_setting_ptr = 1 + phase_setting_ptr p
                        , prev_op = prev_op p
                        , output_value = output_value p
                        }

updatePhaseWithFeedback :: Program -> Program
updatePhaseWithFeedback p = Program { instruction_pointer = instruction_pointer p
                                    , memory = memory p
                                    , phase_setting = phase_setting p
                                    , phase_setting_ptr = case new_ptr of
                                                            5         -> 0
                                                            otherwise -> new_ptr
                                    , prev_op = prev_op p
                                    , output_value = output_value p
                                    }
    where new_ptr = 1 + phase_setting_ptr p                    

updateOutput :: Int -> Program -> Program
updateOutput output_value p = Program { instruction_pointer = instruction_pointer p
                                     , memory = memory p
                                     , phase_setting = phase_setting p
                                     , phase_setting_ptr = phase_setting_ptr p
                                     , prev_op = prev_op p
                                     , output_value = output_value
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
    | in_instruct == 5  = out_instruct JumpIfTrue 3
    | in_instruct == 6  = out_instruct JumpIfFalse 3
    | in_instruct == 7  = out_instruct LessThan 4
    | in_instruct == 8  = out_instruct Equals 4
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
                    Position  -> mem !! (mem !! cur_idx)
                    Immediate -> mem !! cur_idx
                    Write     -> mem !! cur_idx
    where instr_ptr = instruction_pointer p
          cur_idx   = arg_num + instr_ptr
          mem       = memory p


executeInstruction :: Program -> Int -> Program 
executeInstruction p input_val = Program { instruction_pointer = new_instr_ptr
                                         , memory              = new_memory
                                         , prev_op             = op instr
                                         , phase_setting       = phase_setting p
                                         , phase_setting_ptr   = phase_setting_ptr p
                                         , output_value        = case op instr of
                                                                     Out -> arg_1
                                                                     otherwise -> output_value p
                                         }

    where cur_idx = instruction_pointer p
          opcode = parseOpcode $ memory p !! cur_idx
          instr = parseInstruction $ opcode
          arg_1 = parseArg p 1 $ param_1 instr
          arg_2 = parseArg p 2 $ param_2 instr
          arg_3 = parseArg p 3 $ param_3 instr
          write_idx = memory p !! (cur_idx + 1)
          new_memory = case op instr of
                            Add         -> updateList arg_3 (arg_1 + arg_2) $ memory p
                            Mul         -> updateList arg_3 (arg_1 * arg_2) $ memory p
                            Inp         -> updateList write_idx input_val $ memory p
                            LessThan    -> updateList arg_3 (case arg_1 < arg_2 of
                                                        True -> 1
                                                        otherwise -> 0) $ memory p
                            Equals      -> updateList arg_3 (case arg_1 == arg_2 of
                                                        True -> 1
                                                        otherwise -> 0) $ memory p
                            otherwise   -> memory p
          default_ptr   = cur_idx + num_params instr                            
          new_instr_ptr = case op instr of
                            JumpIfTrue  -> case arg_1 of
                                                0         -> default_ptr
                                                otherwise -> arg_2
                            JumpIfFalse -> case arg_1 of
                                                0         -> arg_2
                                                otherwise -> default_ptr
                            otherwise   -> default_ptr

execute :: Program -> Int -> Program
execute p input_val = case prev_op p of
                            Halt      -> p
                            otherwise -> flip execute input_val $ executeInstruction p input_val

executeAmplified :: Int -> Program -> Program 
executeAmplified input_val p
    | phase_setting_ptr p >= 5  = p
    | otherwise                 = executeAmplified next_in p'''
    where 
          current_phase = phase_setting p !! phase_setting_ptr p
          p' = executeInstruction p current_phase -- set up phase
          p'' = execute p' input_val -- run the rest of the program
          next_in = output_value p'' -- extract output from program
          -- track current output and update phase
          p''' = updateOutput next_in $ updatePhase p

executeWithPhase :: [Int] -> Program -> Program
executeWithPhase phase_setting p = executeAmplified 0 $ setPhase phase_setting p


-- TODO: Don't know how to handle base case for feedback
executeFeedback :: Int -> Program -> Program
executeFeedback input_val p
    | phase_setting_ptr p >= 5  = p
    | otherwise                 = executeFeedback next_in p'''
    where 
          current_phase = phase_setting p !! phase_setting_ptr p
          p' = executeInstruction p current_phase -- set up phase
          p'' = execute p' input_val -- run the rest of the program
          next_in = output_value p'' -- extract output from program
          -- track current output and update phase
          p''' = updateOutput next_in $ updatePhase p

executeFeedbackWithPhase :: [Int] -> Program -> Program
executeFeedbackWithPhase phase_setting p = executeFeedback 0 $ setPhase phase_setting p
          

solveP1 :: String -> Int
solveP1 s = maximum $ map (\phase -> output_value $ executeWithPhase phase p) perms
    where p = readCode s
          perms = permutations [0,1,2,3,4]

solveP2 :: String -> Int
solveP2 s = maximum $ map (\phase -> output_value $ executeFeedbackWithPhase phase p) perms
    where p = readCode s
          perms = permutations [5,6,7,8,9]

    

solve :: String -> String
solve input_string = unlines [a,b]
    where a = "Part 1: " ++ (show . solveP1) input_string
          b = "Part 2: TODO" -- TODO ++ (show . solveP2) input_string

main :: IO()
main = interact $ solve
