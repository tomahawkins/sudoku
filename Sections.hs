module Sections
  ( solve
  ) where

import Board
import Section

data Sections = Sections
  { s0, s1, s2, s3, s4, s5, s6, s7, s8 :: Section
  , c0, c1, c2, c3, c4, c5, c6, c7, c8 :: [Constraint]   -- Constraint mailboxes for each section.
  }

-- Initialize all the sections given a board layout.
newSections :: Board -> Sections
newSections (Board s) = Sections
  { s0 = newSection $ map take0 $ take0 s
  , s1 = newSection $ map take1 $ take0 s
  , s2 = newSection $ map take2 $ take0 s
  , s3 = newSection $ map take0 $ take1 s
  , s4 = newSection $ map take1 $ take1 s
  , s5 = newSection $ map take2 $ take1 s
  , s6 = newSection $ map take0 $ take2 s
  , s7 = newSection $ map take1 $ take2 s
  , s8 = newSection $ map take2 $ take2 s
  , c0 = []
  , c1 = []
  , c2 = []
  , c3 = []
  , c4 = []
  , c5 = []
  , c6 = []
  , c7 = []
  , c8 = []
  }
  where
  take0 = take 3
  take1 = take 3 . drop 3
  take2 = drop 6

step :: Sections -> Sections
step = step0 . step1 . step2 . step3 . step4 . step5 . step6 . step7 . step8

step0 a = a { s0 = s, c0 = [], c1 = row ++ c1 a, c2 = row ++ c2 a, c3 = col ++ c3 a, c6 = col ++ c6 a } where (row, col, s) = processSection (c0 a) (s0 a)
step1 a = a { s1 = s, c1 = [], c0 = row ++ c0 a, c2 = row ++ c2 a, c4 = col ++ c4 a, c7 = col ++ c7 a } where (row, col, s) = processSection (c1 a) (s1 a)
step2 a = a { s2 = s, c2 = [], c0 = row ++ c0 a, c1 = row ++ c1 a, c5 = col ++ c5 a, c8 = col ++ c8 a } where (row, col, s) = processSection (c2 a) (s2 a)

step3 a = a { s3 = s, c3 = [], c4 = row ++ c4 a, c5 = row ++ c5 a, c0 = col ++ c0 a, c6 = col ++ c6 a } where (row, col, s) = processSection (c3 a) (s3 a)
step4 a = a { s4 = s, c4 = [], c3 = row ++ c3 a, c5 = row ++ c5 a, c1 = col ++ c1 a, c7 = col ++ c7 a } where (row, col, s) = processSection (c4 a) (s4 a)
step5 a = a { s5 = s, c5 = [], c3 = row ++ c3 a, c4 = row ++ c4 a, c2 = col ++ c2 a, c8 = col ++ c8 a } where (row, col, s) = processSection (c5 a) (s5 a)

step6 a = a { s6 = s, c6 = [], c7 = row ++ c7 a, c8 = row ++ c8 a, c0 = col ++ c0 a, c3 = col ++ c3 a } where (row, col, s) = processSection (c6 a) (s6 a)
step7 a = a { s7 = s, c7 = [], c6 = row ++ c6 a, c8 = row ++ c8 a, c1 = col ++ c1 a, c4 = col ++ c4 a } where (row, col, s) = processSection (c7 a) (s7 a)
step8 a = a { s8 = s, c8 = [], c6 = row ++ c6 a, c7 = row ++ c7 a, c2 = col ++ c2 a, c5 = col ++ c5 a } where (row, col, s) = processSection (c8 a) (s8 a)

processSections :: Sections -> Sections
processSections a
  | all null [c0 b, c1 b, c2 b, c3 b, c4 b, c5 b, c6 b, c7 b, c8 b] = b
  | otherwise                                                       = processSections b
  where
  b = step a

knownBoard :: Sections -> Board
knownBoard a = Board $ f a0 a1 a2 ++ f a3 a4 a5 ++ f a6 a7 a8
  where
  a0 = knownSymbols $ s0 a
  a1 = knownSymbols $ s1 a
  a2 = knownSymbols $ s2 a
  a3 = knownSymbols $ s3 a
  a4 = knownSymbols $ s4 a
  a5 = knownSymbols $ s5 a
  a6 = knownSymbols $ s6 a
  a7 = knownSymbols $ s7 a
  a8 = knownSymbols $ s8 a
  f :: [[a]] -> [[a]] -> [[a]] -> [[a]]
  f a b c = [ a ++ b ++ c | (a, b, c) <- zip3 a b c ]

solve :: Board -> Board
solve = knownBoard . processSections . newSections


