module Game.Chess.Piece where

import qualified Data.Vector as V
import Game.Chess.Types

-- | Section 3, "Programming a Computer for Playing Chess" By Claude E. Shannon.
pointValue :: Piece -> Int
pointValue None = 0
pointValue King = 0 -- I guess?
pointValue Pawn = 1
pointValue Knight = 3
pointValue Bishop = 3
pointValue Rook = 5
pointValue Queen = 9

-- | Given a 'Board' and a 'Color', determine the current piece-point value for
-- that color.
piecePoints :: Board -> Color -> Int
piecePoints b c =
  V.sum .
  V.map (pointValue . piece) .
  V.filter (\cell -> color cell == c) $ board b
