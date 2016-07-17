module Game.Chess.Negamax where

import Game.Chess.Piece
import Game.Chess.Types

-- | (Maybe) an implementation of a negamax algorithm.
--
-- This is the stupidest thing that could possibly work. It does not do
-- alpha-beta pruning or anything along those lines, and thus is stupidly
-- inefficient.
negamax :: Int -> GameTree -> Int
negamax 0 (GameTree brd _) = evaluate brd
negamax depth (GameTree _ brds) =
  let scores = negate . negamax (depth - 1) <$> brds
  in if scores == []
     then (-1000)
     else maximum scores
