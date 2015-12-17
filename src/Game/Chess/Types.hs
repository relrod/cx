module Game.Chess.Types where

import Data.Bits
import Data.Word
import qualified Data.Vector as V

-- I am torn on using a bitboard (or any piece-centric board representation) as
-- opposed to a square-centric representation which seems more intuitive to me.
-- https://chessprogramming.wikispaces.com/Board+Representation
-- So, this might change. Edward Kmett has some thoughts on a bitboard
-- representation here: https://da.gd/RQeJK
--
-- For now, I am using a 0x88 representation, which is still more low-level
-- than I am used to, but nevertheless, should be simple enough to work with.

data Board =
  Board { board :: V.Vector Cell
        , sideToMove :: Color
          -- TODO: Castling ability, and so on.
        } deriving (Eq, Ord, Show)

data Piece =
    None
  | Pawn
  | Knight
  | Bishop
  | King
  | Queen
  | Rook
  deriving (Eq, Ord, Show, Enum)

data Color = White | Black deriving (Eq, Ord, Show, Enum)

data Cell = Cell Piece Color deriving (Eq, Ord)

instance Show Cell where
  show (Cell None _) = " "
  show (Cell Pawn White) = "♙"
  show (Cell Pawn Black) = "♟"
  show (Cell Knight White) = "♘"
  show (Cell Knight Black) = "♞"
  show (Cell Bishop White) = "♗"
  show (Cell Bishop Black) = "♝"
  show (Cell King White) = "♔"
  show (Cell King Black) = "♚"
  show (Cell Queen White) = "♕"
  show (Cell Queen Black) = "♛"
  show (Cell Rook White) = "♖"
  show (Cell Rook Black) = "♜"
