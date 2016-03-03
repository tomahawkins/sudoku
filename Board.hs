module Board
  ( Board (..)
  , Coordinate
  , Sym
  , parseBoard
  ) where

import Data.List

-- | Abstract datatype of a Sudoku board.
data Board = Board [[Maybe Sym]]

-- Sudoku symbols: 1-9.
type Sym = Int

-- | Coordinate of space on board (row, column).  Upper left is (0, 0).
type Coordinate = (Int, Int)

instance Show Board where
  show (Board a) = unlines $ map showRow (take 3          a) ++ ["------+-------+------"] ++
                             map showRow (take 3 $ drop 3 a) ++ ["------+-------+------"] ++
                             map showRow (take 3 $ drop 6 a)
    where
    showRow :: [Maybe Int] -> String
    showRow a = intersperse ' ' $ concatMap showPosition (take 3          a) ++ "|" ++
                                  concatMap showPosition (take 3 $ drop 3 a) ++ "|" ++
                                  concatMap showPosition (take 3 $ drop 6 a)
    showPosition :: Maybe Int -> String
    showPosition a = case a of
      Nothing -> " "
      Just a  -> show a

-- | Parse a string into a Board.
parseBoard :: String -> Board
parseBoard = Board . take 9 . map parseLine . (++ repeat "") . lines
  where
  parseLine :: String -> [Maybe Int]
  parseLine = take 9 . map parsePosition . (++ repeat ' ') 
  parsePosition :: Char -> Maybe Int
  parsePosition a = case a of
    ' ' -> Nothing
    a   -> Just $ read [a]

