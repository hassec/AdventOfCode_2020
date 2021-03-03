import Data.List (delete, intersect, nub)
import Data.List.Split (splitOn)

main = do
  input <- getContents
  let groups = splitOn "\n\n" input
  print $ sum . map (length . delete '\n' . nub) $ groups
  print $ sum $ map (length . foldl1 intersect) . filter (not . null) . map lines $ groups
