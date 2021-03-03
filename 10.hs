import Data.List (sort)
import Data.List.Split (splitOn)

countOccurences :: (Eq a) => a -> [a] -> Int
countOccurences x = length . filter (== x)

-- possible combinations for x adapters
combs :: Int -> Int
combs x = 2 ^ (x - 1)

toFactor :: [Int] -> Int
toFactor [] = 1
toFactor l = case length l of
  1 -> 1
  2 -> 2
  3 -> 4
  4 -> 7

main = do
  input <- getContents

  -- read input, split into array, and transform strings into numbers
  let array = map read $ lines input :: [Int]

  let sorted = sort array
  let full_list = 0 : sorted ++ [last sorted + 3]

  -- create list of differences
  let steps = zipWith (-) (tail full_list) full_list

  -- get distribution
  let diffs = [countOccurences x steps | x <- [1 .. 3]]
  print $ "Part 1 distribution: " ++ show diffs
  print $ "Part 1 solution: " ++ show (head diffs * last diffs)

  -- factorize the problem so it doesn't blow up.
  let l = splitOn [3] steps
  --
  -- cheat a little by knowing that the largest consecutive list of ones is 4 and that there aren't any twos in the list
  let part2 = product $ map toFactor l

  print $ "Part 2: " ++ show part2
