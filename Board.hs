module Board
  ( Board (..)
  , parseBoard
  ) where

import Data.List

-- | Abstract datatype of a Sudoku board.
data Board a = Board [[Maybe a]]

instance Show a => Show (Board a) where
  show (Board a) = unlines $ (take 3          rows) ++ [sep] ++
                             (take 3 $ drop 3 rows) ++ [sep] ++
                             (take 3 $ drop 6 rows)
    where
    sepEdge = replicate (maxLen * 3 + 3) '-'
    sepCenter = '-' : sepEdge
    sep = sepEdge ++ "+" ++ sepCenter ++ "+" ++ sepEdge
    positions' = map (map showPosition) a
    maxLen = maximum $ map length $ concat positions'
    positions :: [[String]]
    positions  = map (map (\ a -> a ++ replicate (maxLen - length a) ' ')) positions'
    rows = map showRow positions
    showRow :: [String] -> String
    showRow a = intercalate " " $ (take 3 a) ++ ["|"] ++ (take 3 $ drop 3 a) ++ ["|"] ++ (take 3 $ drop 6 a)
    showPosition :: Show a => Maybe a -> String
    showPosition a = case a of
      Nothing -> " "
      Just a  -> show a

-- | Parse a string into a Board.
parseBoard :: String -> Board Int
parseBoard = Board . take 9 . map parseLine . (++ repeat "") . lines
  where
  parseLine :: String -> [Maybe Int]
  parseLine = take 9 . map parsePosition . (++ repeat ' ') 
  parsePosition :: Char -> Maybe Int
  parsePosition a = case a of
    ' ' -> Nothing
    a   -> Just $ read [a]

