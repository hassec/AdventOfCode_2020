import qualified Data.HashMap.Strict as HMS
import Data.Maybe (catMaybes, fromJust, isJust)

-- a Seat is free or not so let's use newtype on Bool
-- instead of Data
newtype Seat = Seat {isEmpty :: Bool} deriving (Eq, Show)

type Position = (Int, Int)

type Direction = (Int, Int)

type SeatLayout = HMS.HashMap (Int, Int) Seat

type EvolveSeatFunc = SeatLayout -> Position -> Seat -> Seat

-- should only expect 'L' or '.'
toSeat :: Char -> Maybe Seat
toSeat 'L' = Just $ Seat True
toSeat '.' = Nothing

getLayout :: String -> (SeatLayout, Int, Int)
-- unpacking into (x, Just y) removes the entries that are Nothing
getLayout x = (HMS.fromList [(x, y) | (x, Just y) <- coordsAndSeats], width, height)
  where
    x_l = lines x
    height = length x_l
    width = length . head $ x_l
    coords = [(x, y) | y <- [0 .. height -1], x <- [0 .. width -1]]
    seats = map toSeat $ concat x_l
    coordsAndSeats = zip coords seats

countTakenSeats :: SeatLayout -> Int
countTakenSeats = HMS.foldl' (\x s -> x + toInt s) 0
  where
    toInt = fromEnum . not . isEmpty

evolveLayout :: EvolveSeatFunc -> SeatLayout -> SeatLayout
evolveLayout seatEvolver sl
  | sl2 == sl = sl
  | otherwise = evolveLayout seatEvolver sl2
  where
    sl2 = HMS.mapWithKey (seatEvolver sl) sl

evolveSeatAdj :: SeatLayout -> Position -> Seat -> Seat
evolveSeatAdj sl pos s
  -- no adjacent seats occupied -> seat becomes occupied or stays occupied
  | count == 0 = Seat False
  -- tol or more adjacent seats occupied -> seat becomes empty or stays empty
  | count > 3 = Seat True
  -- seat remains unchanged
  | otherwise = s
  where
    neighbours = catMaybes [HMS.lookup (fst pos + x, snd pos + y) sl | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]
    count = length . filter (== Seat False) $ neighbours

evolveSeatVis :: Int -> Int -> SeatLayout -> Position -> Seat -> Seat
evolveSeatVis maxX maxY sl pos s
  | count == 0 = Seat False
  | count > 4 = Seat True
  | otherwise = s
  where
    -- let's just get the maximum coordinate in x or y and assume a square
    -- don't want to refactor to much now for part2
    --maxXY = maximum $ map maximum (HMS.keys sl)
    neighbours = catMaybes [myLookup pos (x, y) | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]
      where
        myLookup :: Position -> Direction -> Maybe Seat
        myLookup pos (dx, dy)
          | x > maxX || x < 0 || y > maxY || y < 0 = Nothing
          | isJust val = val
          | otherwise = myLookup (x, y) (dx, dy)
          where
            x = fst pos + dx
            y = snd pos + dy
            val = HMS.lookup (x, y) sl
    count = length . filter (== Seat False) $ neighbours


main = do
  (layout, width, height) <- getLayout <$> getContents

  --Part 1
  let p1 = evolveLayout evolveSeatAdj layout
  print $ countTakenSeats p1

  let p2 = evolveLayout (evolveSeatVis width height) layout
  print $ countTakenSeats p2
