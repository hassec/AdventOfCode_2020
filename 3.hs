

-- simply filter select the forest rows into which we step
filterRows :: Int -> [[Char]] -> [[Char]]
filterRows step_down forest = [ f | (f,i) <- zip forest [0..], mod i step_down == 0]

-- first filter with above function depending on step down size "sd"
-- then perform the right steps on each line depending on step right size "sr"
-- this results in a list of encountered objects which we simply filter for trees to get amount of collisions
countCollisions forest width sr sd = length . filter (=='#') $ rightSteps $ filterRows sd forest
    where rightSteps f = [forest_line !! idx | (forest_line, idx) <- zip f (map (`mod` width) [0,sr..]) ]

main = do
  input <- getContents
  let forest = lines input
  let width = length $ head forest

  print $ "Part1: " ++  show (countCollisions forest width 3 1)

  let args = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  let p2 = product $ map (uncurry (countCollisions forest width)) args

  print $ "Part2: " ++  show p2


