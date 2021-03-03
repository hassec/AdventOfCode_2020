import Data.List ((\\))

parse :: Char -> Char -> [Char] -> Int -> Int
parse low high (c : cs) n
  | c == low = 0 + parse low high cs ((n + 1) `div` 2)
  | c == high = let half = (n + 1) `div` 2 in half + parse low high cs half
parse _ _ [] _ = 0

parseCol :: [Char] -> Int -> Int
parseCol = parse 'L' 'R'

parseRow :: [Char] -> Int -> Int
parseRow = parse 'F' 'B'

calcSeatID :: [Char] -> Int
calcSeatID p = let (row, column) = span (\x -> x == 'F' || x == 'B') p in parseRow row 127 * 8 + parseCol column 7

main = do
  input <- getContents
  let passes = lines input

  let seatIds = map calcSeatID passes
  let max = maximum seatIds
  print max

  let leftover = [0 .. max] \\ seatIds
  let mySeat = [x | x <- leftover, (x -1) `notElem` leftover, (x + 1) `notElem` leftover]
  print mySeat
