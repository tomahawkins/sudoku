module Main (main) where

import Data.List (transpose)
import Data.Maybe (fromJust)

import Board
import FDSolver
--import Sections (solve)

main :: IO ()
main = do
  solve1 easy
  solve1 med29
  solve1 med30
  solve1 hard111
  solve1 hard112
  solve1 garfield3
  solve1 garfield4
  solve1 garfield5

solve1 :: Board Int -> IO ()
solve1 a = do
  putStrLn $ unlines [ "    " ++ a ++ "    " ++ b ++ "    " ++ c | (a, b, c) <- zip3 (lines $ show a) (lines $ show b) (lines $ show c) ]
  putStrLn ""
  where
  c@(Board c') = solveBoard a
  b = Board $ map (map $ \ a -> case a of { Nothing -> Nothing; Just [a] -> Just a; Just _ -> Nothing }) c'
  --b = solve a

solveBoard :: Board Int -> Board [Int]
solveBoard a = toBoard $ fromJust $ reduce $ do
  a <- fromBoard a
  mapM_ allConstraints a
  mapM_ allConstraints $ transpose a
  setConstraints $ sections a
  return a

sections :: [[a]] -> [[ [[a]] ]]
sections a = [[s00, s01, s02], [s10, s11, s12], [s20, s21, s22]]
  where
  r0  = transpose $ take 3 a
  r1  = transpose $ take 3 $ drop 3 a
  r2  = transpose $ drop 6 a
  s00 = transpose $ take 3          r0
  s01 = transpose $ take 3 $ drop 3 r0
  s02 = transpose $          drop 6 r0
  s10 = transpose $ take 3          r1
  s11 = transpose $ take 3 $ drop 3 r1
  s12 = transpose $          drop 6 r1
  s20 = transpose $ take 3          r2
  s21 = transpose $ take 3 $ drop 3 r2
  s22 = transpose $          drop 6 r2

fromBoard :: Board Int -> FD Int [[Var]]
fromBoard (Board a) = mapM (mapM $ \ a -> case a of { Nothing -> newVar [1 .. 9]; Just i -> newVar [i] }) a

toBoard :: ([[Var]], Var -> [Int]) -> Board [Int]
toBoard (a, f) = Board $ map (map (Just . f)) a

allConstraints a = do
  allDifferent a
  --allAccounted a

allDifferent :: [Var] -> FD Int ()
allDifferent a = case a of
  [] -> return ()
  a : b -> do
    sequence_ [ assert $ a :/= b | b <- b ]
    allDifferent b

setConstraints :: [[ [[Var]] ]] -> FD Int ()
setConstraints [[s00, s01, s02], [s10, s11, s12], [s20, s21, s22]] = do
  --mapM_ (allDifferent . concat) [s00, s01, s02, s10, s11, s12, s20, s21, s22]
  setConstraintsRow s00 s01 s02
  setConstraintsRow s10 s11 s12
  setConstraintsRow s20 s21 s22
  setConstraintsRow (transpose s00) (transpose s10) (transpose s20)
  setConstraintsRow (transpose s01) (transpose s11) (transpose s21)
  setConstraintsRow (transpose s02) (transpose s12) (transpose s22)
setConstraints _ = undefined

setConstraintsRow :: [[Var]] -> [[Var]] -> [[Var]] -> FD Int ()
setConstraintsRow [a0, a1, a2] [b0, b1, b2] [c0, c1, c2] = do
  assert $ (a1 ++ a2) :$= (b0 ++ c0)
  assert $ (a0 ++ a2) :$= (b1 ++ c1)
  assert $ (a0 ++ a1) :$= (b2 ++ c2)
  assert $ (b1 ++ b2) :$= (a0 ++ c0)
  assert $ (b0 ++ b2) :$= (a1 ++ c1)
  assert $ (b0 ++ b1) :$= (a2 ++ c2)
  assert $ (c1 ++ c2) :$= (a0 ++ b0)
  assert $ (c0 ++ c2) :$= (a1 ++ b1)
  assert $ (c0 ++ c1) :$= (a2 ++ b2)
setConstraintsRow _ _ _ = undefined

allAccounted :: [Var] -> FD Int ()
allAccounted a
  | length a /= 9 = error $ "Invalid length."
  | otherwise = do
      vars <- mapM (newVar . (:[])) [1..9]
      flip mapM_ a $ \ a -> do
        assert $ foldl1 (:||) [ a :== n | n <- vars ]

notIn :: [([Var], [Var])] -> FD Int ()
notIn a = do
  vars <- mapM (newVar . (:[])) [1..9]
  mapM_ (notIn' vars) a

notIn' :: [Var] -> ([Var], [Var]) -> FD Int ()
notIn' vars (a, b) = do
  flip mapM_ vars $ \ var -> do
    assert $ foldl1 (:&&) [ a :/= var | a <- a ] :-> foldl1 (:&&) [ b :/= var | b <- b ]
    assert $ foldl1 (:&&) [ b :/= var | b <- b ] :-> foldl1 (:&&) [ a :/= var | a <- a ]

easy :: Board Int
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

med29 :: Board Int
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

med30 :: Board Int
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

hard111 :: Board Int
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

hard112 :: Board Int
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

garfield3 :: Board Int
garfield3 = parseBoard $ unlines
  [ "  91 63  "
  , " 6  4 9  "
  , "1  93 2  "
  , "4  38    "
  , " 81 6 42 "
  , "    94  8"
  , "  7 19  4"
  , "  4 2  5 "
  , "  64 38  "
  ]

garfield4 :: Board Int
garfield4 = parseBoard $ unlines
  [ "69  8  4 "
  , "2   5   8"
  , " 5    2 9"
  , " 6 7  4 3"
  , "   431   "
  , "4 3  5 7 "
  , "7 2    5 "
  , "9   1   7"
  , " 3  6  24"
  ]

garfield5 :: Board Int
garfield5 = parseBoard $ unlines
  [ "1  2 3   "
  , "  21 9 7 "
  , "    7   8"
  , " 916     "
  , "  4 2 6  "
  , "     781 "
  , "2   6    "
  , " 5 3 87  "
  , "   9 2  5"
  ]

