import Data.Char
import Data.List

main = do
  input <- getContents
  let parsed_input = map transform $ lines input
        where
          transform = filter (any isAlphaNum) . groupBy of_same_char_category
          of_same_char_category = (\x y -> generalCategory x == generalCategory y)

  let a = length . filter char_count_in_limit $ parsed_input
        where
          char_count_in_limit [min, max, char, string] = count_char_in_word char string `elem` [read min .. read max]
          count_char_in_word c word = length . filter (== head c) $ word
  print a

  let x = length . filter password_valid $ parsed_input
        where
          password_valid [min, max, char, string] = length (filter (== head char) [string !! (read min -1), string !! (read max -1)]) == 1

  print x
