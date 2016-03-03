module Section
  ( Section
  , Constraint
  , newSection
  , processSection
  , knownSymbols
  ) where

import Data.List

import Board (Sym)

-- Keeps track of the knowledge base and sent outgoing constraints of a section.
data Section = Section Knowledge [Constraint] deriving Show

-- Section knowledge.  Possible symbols for each position in a section (0 - 8).
type Knowledge = [[Sym]]

-- Constraints that are passed between sections.
data Constraint
  = NotInRow0 Sym
  | NotInRow1 Sym
  | NotInRow2 Sym
  | NotInCol0 Sym
  | NotInCol1 Sym
  | NotInCol2 Sym
  deriving (Show, Eq)

-- Initialize a new section given the known section's symbols.
newSection :: [[Maybe Sym]] -> Section
newSection a = Section k []
  where
  k = map f $ concat a
  f :: Maybe Sym -> [Int]
  f a = case a of
    Just a  -> [a]
    Nothing -> [1 .. 9]

-- Given a new list of incoming constraints, process a section,
-- returning outgoing row and column contraints and the updated section.
processSection :: [Constraint] -> Section -> ([Constraint], [Constraint], Section)
processSection inputConstraints (Section knowledge propogated) = (row, col, Section knowledge' $ row ++ col ++ propogated)
  where
  knowledge' = refineKnowledge $ foldr updateKnowledge knowledge inputConstraints
  (row, col) = partition isRow $ outputConstraints knowledge' \\ propogated
  isRow :: Constraint -> Bool
  isRow a = case a of
    NotInRow0 _ -> True
    NotInRow1 _ -> True
    NotInRow2 _ -> True
    _ -> False

-- Return the known symbols so far.
knownSymbols :: Section -> [[Maybe Sym]]
knownSymbols (Section a _) = map (map f) [take 3 a, take 3 $ drop 3 a, drop 6 a]
  where
  f :: [Sym] -> Maybe Sym
  f a = case a of
    [a] -> Just a
    _   -> Nothing



-- Given a constraint, update the knowledge base.
updateKnowledge :: Constraint -> Knowledge -> Knowledge
updateKnowledge a b = case a of
  NotInRow0 sym -> f [0, 1, 2] sym
  NotInRow1 sym -> f [3, 4, 5] sym
  NotInRow2 sym -> f [6, 7, 8] sym
  NotInCol0 sym -> f [0, 3, 6] sym
  NotInCol1 sym -> f [1, 4, 7] sym
  NotInCol2 sym -> f [2, 5, 8] sym
  where
  f :: [Int] -> Sym -> Knowledge
  f range sym = [ if elem i range then filter (/= sym) a else a | (i, a) <- zip [0 ..] b ]

-- Refine knowledge base based on local section information.
refineKnowledge :: Knowledge -> Knowledge
refineKnowledge k
  | k == k1   = k1
  | otherwise = refineKnowledge k1
  where
  k1 = refineSingles . refineKnown $ k

-- Remove options that are known.
refineKnown :: Knowledge -> Knowledge
refineKnown k = map f k
  where
  known = [ a | [a] <- k ]
  f :: [Sym] -> [Sym]
  f a = case a of
    [_] -> a
    a   -> filter (flip notElem known) a

-- If an option only appears once in a section, it's known.
refineSingles :: Knowledge -> Knowledge
refineSingles k = map f k
  where
  singles = [ i | i <- [1 .. 9], length (elemIndices i $ concat k) == 1 ] 
  f a
    | null (intersect a singles) = a
    | otherwise                  = intersect a singles

-- Build a list of outward constraints given current knowledge of a section.
outputConstraints :: Knowledge -> [Constraint]
outputConstraints k = concatMap f [1 .. 9]
  where
  f :: Sym -> [Constraint]
  f sym = row ++ col
    where
    i = findIndices (elem sym) k
    row0 = all (flip elem [0, 1, 2]) i
    row1 = all (flip elem [3, 4, 5]) i
    row2 = all (flip elem [6, 7, 8]) i
    col0 = all (flip elem [0, 3, 6]) i
    col1 = all (flip elem [1, 4, 7]) i
    col2 = all (flip elem [2, 5, 8]) i
    row
      | row0      = [NotInRow0 sym]
      | row1      = [NotInRow1 sym]
      | row2      = [NotInRow2 sym]
      | otherwise = []
    col
      | col0      = [NotInCol0 sym]
      | col1      = [NotInCol1 sym]
      | col2      = [NotInCol2 sym]
      | otherwise = []
      
