type Position = (Double, Double)

type Waypoint = (Double, Double)

type Heading = Int

type ShipConfig1 = (Heading, Position)

type ShipConfig2 = (Position, Waypoint)

type Command = String

toRad :: Int -> Double
toRad x = fromIntegral x / 180.0 * pi

part1 :: ShipConfig1 -> Command -> ShipConfig1
part1 (heading, (north, east)) (com : num) = case com of
  'N' -> (heading, (north + d, east))
  'E' -> (heading, (north, east + d))
  'S' -> (heading, (north - d, east))
  'W' -> (heading, (north, east - d))
  'L' -> ((heading + 360 - rot) `mod` 360, (north, east))
  'R' -> ((heading + rot) `mod` 360, (north, east))
  'F' -> (heading, (north + d * cos (toRad heading), east + d * sin (toRad heading)))
  where
    n = read num :: Int
    d = fromIntegral n :: Double
    rot = n `mod` 360 -- if someone says L400 we want to tranform that into L40 such that our (heading +  360 - rot) logic works

-- simple 2D rotation
--             | cos(a)  -sin(a)|
-- vec x' =    |                | * vec x
--             | sin(a)   cos(a)|
rotate :: Int -> Waypoint -> Waypoint
rotate deg (w_n, w_e) = let rad = toRad deg in (cos rad * w_n - sin rad * w_e, sin rad * w_n + cos rad * w_e)

part2 :: ShipConfig2 -> Command -> ShipConfig2
part2 ((north, east), (w_n, w_e)) (com : num) = case com of
  'N' -> ((north, east), (w_n + d, w_e))
  'E' -> ((north, east), (w_n, w_e + d))
  'S' -> ((north, east), (w_n - d, w_e))
  'W' -> ((north, east), (w_n, w_e - d))
  'L' -> ((north, east), rotate (360 - rot) (w_n, w_e))
  'R' -> ((north, east), rotate rot (w_n, w_e))
  'F' -> ((north + d * w_n, east + d * w_e), (w_n, w_e))
  where
    n = read num :: Int
    d = fromIntegral n :: Double
    rot = n `mod` 360 -- if someone says L400 we want to tranform that into L40 such that our (heading +  360 - rot) logic works

dist :: Position -> Double
dist (n, e) = abs n + abs e

main = do
  command_list <- words <$> getContents

  let final = foldl part1 (90, (0, 0)) command_list
  print final
  -- since we support arbitrary rotations our result here will be double
  -- lets just round it to int, we surely won't have accumulated enough floating point error to be off
  -- far enough that the round will yield the wrong result
  print $ round $ dist $ snd final

  let final2 = foldl part2 ((0, 0), (1, 10)) command_list
  print final2
  print $ round $ dist $ fst final2
