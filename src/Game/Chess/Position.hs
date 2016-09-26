module Game.Chess.Position where

import Data.List
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Vector as V
import Game.Chess.Board (index, everyPiece, move)
import Game.Chess.Piece (movingVectors', multiMovePiece)
import Game.Chess.Types hiding (piece, color)
import Game.Chess.Negamax

-- | Given a position vector in the form (file, rank), add it to a given
-- 'Position', and return the result so long as we remain in a valid 'Position'.
movePosition :: (Int, Int) -> Position -> Maybe Position
movePosition (x, y) (Position (PFile f) (PRank r)) =
  mkPosition (mkFile (f + x)) (mkRank (r + y))
movePosition _ _ = Nothing
{-# INLINE movePosition #-}

-- | Given a position vector in the form (file, rank), \"slide\" the piece to
-- each square recursively in the direction of that vector. Note that here we
-- don't take into account whether or not the move is actually valid, other than
-- ensuring that we remain on the board.
apVector :: (Int, Int) -> Position -> [Position]
apVector v pos = doApVector pos []
  where
    doApVector pos' acc =
      case movePosition v pos' of
        Just newPos -> doApVector newPos (newPos : acc)
        Nothing -> reverse acc

-- | Given a list of moving vectors and a position, return a list of lists,
-- where each inner list contains possible (unvalidated) moves in each possible
-- direction.
apVectors :: [(Int, Int)] -> Position -> [[Position]]
apVectors vs pos = (`apVector` pos) <$> vs

-- | Given a 'Board' and a list of unvalidated 'Positions' we might want to move
-- to, validate each position and see if we can, in fact, move there. In the
-- future, the validator will need to know the kind of 'Piece' being moved so it
-- can check for things like the moving side being in check.
--
-- This function lets us move up until a move becomes invalid.
getValidSlidingMoves :: Board -> [Position] -> [Position]
getValidSlidingMoves brd ps = helper ps []
  where
    helper [] acc = acc
    helper (x:xs) acc =
      case validateMove x brd of
        EmptySquare -> helper xs (x:acc)
        Occupied -> acc
        Take -> x:acc

-- | Given a 'Board', a list of moving vectors, and the 'Position' that we are
-- currently at, return a list of validated positions to which we can move.
--
-- This basically just combines all of the above machinery into an easy-to-use
-- function that we can use in 'generate' below.
getValidMoves :: Board -> [(Int, Int)] -> Position -> [Position]
getValidMoves brd vs pos = apVectors vs pos >>= getValidSlidingMoves brd

-- | Given a 'Board', an origin 'Position', a query 'Position' and a list of
-- moving vectors, determine whether or not the query 'Position' is currently
-- under attack.
--
-- We generate a list of valid moves, and simply determine if the query position
-- is one of those moves. Note that this does _NOT_ yet account for pawn-takes.
--
-- This can be specialized to test for check.
isAttacked :: Board -> Position -> Position -> [(Int, Int)] -> Bool
isAttacked brd origin query vs = any (== query) (getValidMoves brd vs origin)

-- | Can the given move legally be made?
--
-- Note that this is *not* a comprehensive check! In particular, it checks only
-- the following things:
--
--   * The moving side cannot capture its own piece.
--   * (TODO!) A king cannot be captured.
--   * (TODO!) The move will not place the moving side in check.
validateMove :: Position -> Board -> MoveValidity
validateMove p2 brd =
  case index brd p2 of
    Empty -> EmptySquare
    Cell _ c ->
      if c == sideToMove brd
      then Occupied
      else Take

-- | Move generation!
generate :: Board -> Position -> [Board]
generate brd pos =
  case index brd pos of
    Empty -> []
    Cell piece color ->
      -- If it's a sliding piece, then get all the valid positions. Otherwise,
      -- just apply the list of moving vectors to the position.
      let vectors = mkTuple <$> movingVectors' piece color
          moves = if multiMovePiece piece
                  then getValidMoves brd vectors pos
                  else catMaybes $ fmap (`movePosition` pos) vectors
      in fmap (\m -> move pos m brd) moves

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
  in root $ maximumBy (comparing $ negamax (depth - 1) (sideToMove brd)) brds
