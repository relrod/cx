module Game.Chess.Piece where

import qualified Data.Vector as V
import Game.Chess.Types

-- | Section 3, "Programming a Computer for Playing Chess" By Claude E. Shannon.
pointValue :: Piece -> Int
pointValue King = 200
pointValue Pawn = 1
pointValue Knight = 3
pointValue Bishop = 3
pointValue Rook = 5
pointValue Queen = 9

-- | Given a 'Board' and a 'Color', determine the current piece-point value for
-- that color.
piecePoints :: Board -> Color -> Int
piecePoints b c =
  sum . map (piecePoints' b c) $ [King, Queen, Rook, Knight, Bishop, Pawn]

-- | Given a 'Board', a 'Color', and a 'Piece' to filter for, determine the
-- current piece-point value for that color\'s pieces.
piecePoints' :: Board -> Color -> Piece -> Int
piecePoints' b c p =
  V.sum .
  V.map (pointValue . piece) .
  V.filter (\cell -> isCell cell && color cell == c && piece cell == p) $
  board b
  where
    isCell (Cell _ _) = True
    isCell _          = False

-- | Section 3, "Programming a Computer for Playing Chess" By Claude E. Shannon.
materialScore :: Board -> Int
materialScore brd =
  sum . map positionalPointValue $ [King, Queen, Rook, Knight, Bishop, Pawn]
  where
    positionalPointValue p =
      pointValue p * (piecePoints' brd White p -
                      piecePoints' brd Black p)
