import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)



processRule :: [Char] -> ( [Char], [ ( Int, [Char] ) ] )
processRule r = let [k,v] = splitOn "contain" r in
                let key = unwords . init . words $ k in
                let values = map ( init . words ) $ filter ( not . ( "no other bags" `isInfixOf` ) ) . splitOn "," $ v in
                (key, [ (count, values) | l <- values, let count = read . head $ l, let values = unwords . tail $ l])


checkEntry _ [] = False
checkEntry mymap ((_,x):xs)
    | x == "shiny gold" = True
    | otherwise = checkEntry mymap xs || checkEntry mymap (fromJust (Map.lookup x mymap))

countBags _ [] = 0
countBags mymap ((c,x):xs) = c + c * countBags mymap (fromJust (Map.lookup x mymap)) + countBags mymap xs

main = do
  input <- getContents

  let inp = lines input
  let list = map processRule inp

  -- Part 1
  let rules = Map.fromList list
  let check = checkEntry rules
  let num = length . filter (==True) $ map check (Map.elems rules)

  print num

  -- Part 2
  let counter = countBags rules
  let start = fromJust (Map.lookup "shiny gold" rules)

  print $ counter start

