-- | A simple FEN parser using attoparsec.
module Game.Chess.FEN where

import Data.Attoparsec.Text
import Data.Char (digitToInt, isDigit, isLower, isUpper, toLower)
import Data.List (elem, intercalate)
import Game.Chess.Types

charToCells :: Char -> [Cell]
charToCells c
  | isDigit c = replicate (digitToInt c) Empty
  | isLower c = [cellify (toLower c) Black]
  | isUpper c = [cellify (toLower c) White]
  | otherwise = []
  where
    cellify 'p' = Cell Pawn
    cellify 'r' = Cell Rook
    cellify 'n' = Cell Knight
    cellify 'k' = Cell King
    cellify 'q' = Cell Queen
    cellify 'b' = Cell Bishop

parseCell :: Parser Char
parseCell =
  choice $ map char "12345678" ++ map char "PNBRQKpnbrqk"

parseRanks :: Parser [Cell]
parseRanks = do
  ranks <- many1 parseCell `sepBy1` char '/'
  let parsedRanks = map (concatMap charToCells) (reverse ranks)
  return $ intercalate (replicate 8 Empty) parsedRanks ++
    replicate 8 Empty

parseActiveColor :: Parser Color
parseActiveColor = do
  color <- choice [char 'b', char 'w']
  if color == 'b'
    then return Black
    else return White
