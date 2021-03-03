
-- technically head isn't safe here but it's my first haskell endavour so let's just leave it like this for now
part1 :: [Int] -> Int
part1 input = head [ x*y | x<- input, y <-input, x+y==2020]

part2 :: [Int] -> Int
part2 input = head [ x*y*z | x<- input, y <-input, z<-input, x+y+z==2020]


main = do
  input <- getContents
  let array = map read $ lines input :: [Int]
  print $ part1 array
  print $ part2 array
