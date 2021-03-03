import Data.List (find)
import qualified Data.Set as Set

-- parse numbers that are prefixed by + or -
myread :: [Char] -> Int
myread x = if head x == '-' then read x else read . tail $ x

-- op == operator
-- val == value to operator
-- cnt == current value of accumulator
-- ip == current instruction pointer
-- swp == index at which nop and jmp are swaped
-- This function modifies cnt and ip according to the operator and value while checking for possible swap in ip==swp
proccesInst :: [[Char]] -> Int -> Int -> Int -> (Int, Int)
proccesInst [op, val] cnt ip swp = case op of
  "nop" -> if ip /= swp then (cnt, ip + 1) else (cnt, ip + myread val)
  "jmp" -> if ip /= swp then (cnt, ip + myread val) else (cnt, ip + 1)
  "acc" -> (cnt + myread val, ip + 1)

-- cnt == accumulator value
-- ip == insturction pointer
-- code == list of operator and value pairs (puzzle input)
-- state == Set of previousliy encounterd insturction pointers
-- swp == index into code at which an insturction might be flipped
run :: Int -> Int -> [[[Char]]] -> Set.Set Int -> Int -> (Bool, Int)
run cnt ip code state swp
  | Set.member ip state = (False, cnt)
  | ip > length code = (False, cnt)
  | ip == length code = (True, cnt)
  | otherwise = run new_cnt new_ip code (Set.insert ip state) swp
  where
    inst = code !! ip
    (new_cnt, new_ip) = proccesInst inst cnt ip swp

main = do
  input <- getContents

  let inp = map words $ lines input

  let part1 = run 0 0 inp Set.empty (-1)
  print part1

  -- since we don't swap acc we can skip those
  let part2 = find fst [run 0 0 inp Set.empty x | x <- [0 .. length inp -1], "acc" /= head (inp !! x)]
  print part2
