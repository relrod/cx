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
        , castleAbility :: [CastleAbility]
        , enPassant :: Maybe String -- TODO: 'String' is bad
        , halfmoves :: Integer
        , fullmoves :: Integer
        } deriving (Eq, Ord, Show)

data CastleAbility =
    Kingside Color
  | Queenside Color
  deriving (Eq, Ord, Show)

data Piece =
    Pawn
  | Knight
  | Bishop
  | King
  | Queen
  | Rook
  deriving (Eq, Ord, Show, Enum)

data Color = White | Black deriving (Eq, Ord, Show, Enum)

data Cell = Empty | Cell { piece :: Piece, color :: Color } deriving (Eq, Ord)

data Position = Position !Int !Int deriving (Eq, Ord, Show)

instance Show Cell where
  show Empty = " "
  show (Cell Pawn White) = "P"
  show (Cell Pawn Black) = "p"
  show (Cell Knight White) = "N"
  show (Cell Knight Black) = "n"
  show (Cell Bishop White) = "B"
  show (Cell Bishop Black) = "b"
  show (Cell King White) = "K"
  show (Cell King Black) = "k"
  show (Cell Queen White) = "Q"
  show (Cell Queen Black) = "q"
  show (Cell Rook White) = "R"
  show (Cell Rook Black) = "r"
