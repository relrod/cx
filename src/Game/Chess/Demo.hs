-- | Just a silly module to make it easy to test given a parsable FEN string.
module Game.Chess.Demo where

import Data.Attoparsec.Text
import qualified Data.Text as T
import Game.Chess.FEN
import Game.Chess.Position
import Game.Chess.Types

fenSuggestion :: Int -> T.Text -> Board
fenSuggestion depth fen =
  let Right brd = parseOnly parseFEN fen
  in generateSuggestion brd depth
