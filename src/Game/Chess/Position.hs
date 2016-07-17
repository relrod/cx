module Game.Chess.Position where

import Data.List
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Vector as V
import Game.Chess.Board (index, everyPiece, move)
import Game.Chess.Piece (movingVectors')
import Game.Chess.Types hiding (piece, color)
import Game.Chess.Negamax

-- | Given a position vector in the form (file, rank), add it to a given
-- 'Position', and return the result so long as we remain in a valid 'Position'.
movePosition :: (Int, Int) -> Position -> Maybe Position
movePosition (x, y) (Position (PFile f) (PRank r)) =
  mkPosition (mkFile (f + x)) (mkRank (r + y))
movePosition _ _ = Nothing
{-# INLINE movePosition #-}

-- | Can the given move legally be made?
--
-- Note that this is *not* a comprehensive check! In particular, it checks only
-- the following things:
--
--   * The moving side cannot capture its own piece.
--   * (TODO!) The move will not place the moving side in check.
validateMove :: Position -> Position -> Board -> Bool
validateMove p1 p2 brd =
  case index brd p2 of
    Empty -> True
    Cell _ c -> c /= (sideToMove brd)

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
      if validateMove pos newPos brd
      then return (move pos newPos brd)
      else Nothing

-- | Determine possible moves for the side to move.
allMoves :: Board -> [Board]
allMoves brd = piecePositions >>= generate brd
  where
    piecePositions = V.toList . V.map fst $ everyPiece brd (sideToMove brd)

-- | Generate a 'GameTree' by calling 'allMoves' the appropriate number of times
-- depending on the @depth@.
movesTree :: Board -> Int -> GameTree
movesTree brd 0 = GameTree brd []
movesTree brd depth =
  GameTree brd (map (\x -> movesTree x (depth - 1)) (allMoves brd))

generateSuggestion :: Board -> Int -> Board
generateSuggestion brd depth =
  let (GameTree _ brds) = movesTree brd depth
  in root $ maximumBy (comparing $ negamax depth) brds
