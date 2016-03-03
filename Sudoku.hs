module Main (main) where

import Board
import Sections

main :: IO ()
main = do
  solve' easy
  solve' med29
  solve' med30
  solve' hard111
  solve' hard112

solve' :: Board -> IO ()
solve' a = do
  putStrLn $ unlines [ "    " ++ a ++ "    " ++ b | (a, b) <- zip (lines $ show a) (lines $ show b) ]
  putStrLn ""
  where
  b = solve a

easy :: Board
easy = parseBoard $ unlines
  [ "9  5  7 2"
  , " 2 97 8  "
  , "  68   15"
  , "  2 5  36"
  , " 5 3 7 9 "
  , "84  9 5  "
  , "31   56  "
  , "  9 23 8 "
  , "2 4  9  7"
  ]

med29 :: Board
med29 = parseBoard $ unlines
  [ " 87   5  "
  , "    48  7"
  , "4 9  7 18"
  , "   6  854"
  , " 5  7  9 "
  , "946  2   "
  , "29    7 5"
  , "1  79    "
  , "  3   16 "
  ]

med30 :: Board
med30 = parseBoard $ unlines
  [ "   95 8 1"
  , " 5   4  2"
  , " 14  7 5 "
  , "59 7    3"
  , "  8 9 5  "
  , "6    8 19"
  , " 4 3  62 "
  , "7  4   3 "
  , "3 5 29   "
  ]

hard111 :: Board
hard111 = parseBoard $ unlines
  [ "  3  62 7"
  , " 9 7 2   "
  , "87  3    "
  , "2  8    4"
  , " 8  2  3 "
  , "5    9  8"
  , "    9  43"
  , "   3 7 6 "
  , "3 16  7  "
  ]

hard112 :: Board
hard112 = parseBoard $ unlines
  [ "  54    7"
  , " 9 3   65"
  , "3      2 "
  , "  95  6  "
  , "7   4   9"
  , "  6  15  "
  , " 8      1"
  , "96   7 5 "
  , "5    98  "
  ]

