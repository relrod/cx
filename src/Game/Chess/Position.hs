module Game.Chess.Position where

import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Game.Chess.Board (index, everyPiece, move)
import Game.Chess.Piece (movingVectors')
import Game.Chess.Types hiding (piece, color)

-- | Given a position vector in the form (file, rank), add it to a given
-- 'Position', and return the result so long as we remain in a valid 'Position'.
movePosition :: (Int, Int) -> Position -> Maybe Position
movePosition (x, y) (Position (PFile f) (PRank r)) =
  mkPosition (mkFile (f + x)) (mkRank (r + y))
movePosition _ _ = Nothing
{-# INLINE movePosition #-}

-- | Move generation!
generate :: Board -> Position -> [Board]
generate brd pos =
  case index brd pos of
    Empty -> []
    Cell piece color ->
      catMaybes $ fmap doMovePosition (movingVectors' piece color)
  where
    doMovePosition vect = do
      newPos <- movePosition vect pos
      return (move pos newPos brd)

-- | Determine possible moves for a given side.
allMoves :: Color -> Board -> [Board]
allMoves c brd = piecePositions >>= generate brd
  where
    piecePositions = V.toList . V.map fst $ everyPiece brd c

-- | Generate a 'GameTree' by calling 'allMoves' the appropriate number of times
-- depending on the @depth@.
movesTree :: Color -> Board -> Int -> GameTree
movesTree _ brd 0 = GameTree brd []
movesTree c brd depth =
  GameTree brd (map (\x -> movesTree c x (depth - 1)) (allMoves c brd))
