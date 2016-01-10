module Game.Chess.Position where

import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Game.Chess.Board (index, move)
import Game.Chess.Piece (movingVectors)
import Game.Chess.Types

-- | Given a position vector in the form (file, rank), add it to a given
-- 'Position', and return the result so long as we remain in a valid 'Position'.
movePosition :: (Int, Int) -> Position -> Maybe Position
movePosition (x, y) (Position (PFile f) (PRank r)) =
  mkPosition (mkFile (f + x)) (mkRank (r + y))
{-# INLINE movePosition #-}

-- | Move generation!
generate :: Board -> Position -> [Board]
generate brd pos =
  case index brd pos of
    Empty -> []
    Cell piece color -> catMaybes $ fmap doMovePosition (movingVectors piece)
  where
    doMovePosition vect = do
      newPos <- movePosition vect pos
      return (move pos newPos brd)
