module Game.Chess.Negamax where

import Game.Chess.Piece
import Game.Chess.Types

-- | (Maybe) an implementation of a negamax algorithm.
--
-- This is the stupidest thing that could possibly work. It does not do
-- alpha-beta pruning or anything along those lines, and thus is stupidly
-- inefficient.
negamax :: Int -> Color -> GameTree -> Int
negamax 0 _ (GameTree brd _) = evaluate brd
negamax depth c (GameTree _ brds) =
  let scores = negate . negamax (depth - 1) (otherColor c) <$> brds
  in maximum scores
  where
    otherColor White = Black
    otherColor Black = White
