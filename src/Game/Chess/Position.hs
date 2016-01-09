module Game.Chess.Position where

import qualified Data.Vector as V
import Game.Chess.Types

-- | Given a position vector in the form (file, rank), add it to a given
-- 'Position', and return the result so long as we remain in a valid 'Position'.
movePosition :: (Int, Int) -> Position -> Maybe Position
movePosition (x, y) (Position f r) =
  mkPosition (mkFile (getFile f + x)) (mkRank (getRank r + y))
{-# INLINE movePosition #-}
