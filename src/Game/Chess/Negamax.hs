module Game.Chess.Negamax where

import Game.Chess.Piece
import Game.Chess.Position
import Game.Chess.Types

-- | (Maybe) an implementation of a negamax algorithm.
--
-- This is the stupidest thing that could possibly work. It does not do
-- alpha-beta pruning or anything along those lines, and thus is stupidly
-- inefficient.
negamax :: Int -> Board -> Int
negamax 0 brd = evaluate brd
negamax depth brd =
  case allMoves (sideToMove brd) brd of
    [] -> evaluate brd
    moves -> maximum . fmap (negate . negamax (depth - 1)) $ moves
