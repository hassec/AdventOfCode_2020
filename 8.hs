--import Data.List (isInfixOf)
import qualified Data.Set as Set
import Debug.Trace (trace)


myread :: [Char] -> Int
myread x = if head x == '-' then read x else read . tail $ x

proccesInst [op, val] cnt ip = case op of "nop" -> (cnt, ip + 1)
                                          "acc" -> (cnt + myread val, ip+1)
                                          "jmp" -> (cnt, ip + myread val)


run cnt ip code state
    | Set.member ip state = cnt
    | otherwise = run new_cnt new_ip code (Set.insert ip state)
                where inst = code !! ip
                      (new_cnt, new_ip) = proccesInst inst cnt ip



main = do
  input <- getContents

  let inp = map words $ lines input

  let c = run 0 0 inp Set.empty

  print c
  --print inp

