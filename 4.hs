import Data.List (isInfixOf, find)
import Data.Maybe (isJust)
import Data.Char (isNumber)
import Data.List.Split (splitOn)

keywords :: [[Char]]
keywords = ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

eclList :: [[Char]]
eclList = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]


testPassport :: [Char] -> Bool
testPassport pass = all (`isInfixOf` pass) keywords

testHeight :: ([Char], [Char]) -> Bool
testHeight (val, unit) = case unit of "cm" -> let x = read val in x >= 150 && x <= 193
                                      "in" -> let x = read val in x >= 59 && x <= 76
                                      _   -> False

testHairColor:: [Char] -> Bool
testHairColor val = case val of (x:xs) -> x == '#' && length xs == 6 &&  all (`elem` ['0'..'9']++['a'..'f']) xs
                                _ -> False

--byr (Birth Year) - four digits; at least 1920 and at most 2002.
--iyr (Issue Year) - four digits; at least 2010 and at most 2020.
--eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
--hgt (Height) - a number followed by either cm or in:
--If cm, the number must be at least 150 and at most 193.
--If in, the number must be at least 59 and at most 76.
--hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
--ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
--pid (Passport ID) - a nine-digit number, including leading zeroes.
--cid (Country ID) - ignored, missing or not.
testEntry :: [[Char]] -> Bool
testEntry [key, val] = case key of "byr" -> let x = read val in x >= 1920 && x <= 2002
                                   "iyr" -> let x = read val in x >= 2010 && x <= 2020
                                   "eyr" -> let x = read val in x >= 2020 && x <= 2030
                                   "hgt" -> testHeight $ span isNumber val
                                   "hcl" -> testHairColor val
                                   "ecl" -> isJust . find (==val) $ eclList
                                   "pid" -> length val == 9 && all isNumber val
                                   "cid" -> True
                                   _ -> False


checkPassport :: [Char] -> Bool
checkPassport p = let vals = map (splitOn ":") $ words p in
                  all testEntry vals


part2 :: [String] -> Int
part2 passports = length . filter (==True) $ map checkPassport passports


main = do
  input <- getContents
  let passports = map (unwords . lines) $ splitOn "\n\n" input
  let isvalid = map testPassport passports

  let part1 = [x | (x,valid) <- zip passports isvalid , valid]
  print $ length part1

  print $ part2 part1
