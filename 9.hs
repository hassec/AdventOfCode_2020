import Data.List (tails, inits)


uniquePairSum :: [Int] -> [Int]
uniquePairSum l = [ x + y | (x:ys) <- tails l, y <- ys]

check :: Int -> [Int] -> (Bool, Int)
check x xs = (x `elem` uniquePairSum xs, x)

main = do
  input <- getContents

  -- preamble length
  let n = 25

  -- read input, split into array, and transform strings into numbers
  let array = map read $ lines input :: [Int]
  -- drop the first n numbers which represent the preamble, remainder are the numbers which we need to test
  let numsToTest = drop n array
  -- Creates a list of all possible preambles.
  let pres = map (take n) $ tails array

  let num =  snd . head $ filter (not . fst) $ zipWith check numsToTest pres
  print $ "Part 1: " ++ show num

  let x =  head [ x | x <- concatMap inits $ tails array, sum x == num ]
  print $ "Part 2: " ++ show (maximum x + minimum x)
