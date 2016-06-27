{-# LANGUAGE PatternSynonyms #-}
module Game.Chess.Types (
    Board (..)
  , CastleAbility (..)
  , Piece (..)
  , Color (..)
  , Cell (..)
  , File
  , Rank
  , pattern PRank
  , pattern PFile
  , Position (..)
  , getFile
  , getRank
  , GameTree (..)

  -- * Smart constructors
  , mkFile
  , mkRank
  , mkPosition
) where

import Data.Bits
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

-- | Describes a 'File' on the board.
newtype File = File { getFile :: Int } deriving (Eq, Ord)

-- | Describes a 'Rank' on the board.
newtype Rank = Rank { getRank :: Int } deriving (Eq, Ord)

pattern PFile :: Int -> File
pattern PFile a <- File a

pattern PRank :: Int -> Rank
pattern PRank a <- Rank a

-- | 'Position' allows us to discuss a position on the board, given its
-- appropriate 'File' and 'Rank'. We define an 'Enum' instance for easily
-- converting to and from the 0x88 'V.Vector' position of a 'Board'\'s 'board'
-- representation.
data Position = Position !File !Rank deriving (Eq, Ord, Show)

-- | 'GameTree' represents a tree of game states. Typically it is constructed
-- around the same time move generation happens.
data GameTree =
  GameTree { root :: Board
           , gameTree :: [GameTree]
           } deriving (Eq, Ord, Show)

-- | A smart constructor for 'File' that ensures that the file is between
-- @0@ and @7@ inclusive.
mkFile :: Int -> Maybe File
mkFile f
  | f >= 0 && f < 8 = Just (File f)
  | otherwise       = Nothing

-- | A smart constructor for 'Rank' that ensures that the rank is between
-- @0@ and @7@ inclusive.
mkRank :: Int -> Maybe Rank
mkRank r
  | r >= 0 && r < 8 = Just (Rank r)
  | otherwise       = Nothing

-- | A helper for constructing 'Position', useful in conjunction with the smart
-- constructors 'mkFile' and 'mkRank'.
mkPosition :: Maybe File -> Maybe Rank -> Maybe Position
mkPosition f r = Position <$> f <*> r

instance Show File where
  show (File 0) = "A"
  show (File 1) = "B"
  show (File 2) = "C"
  show (File 3) = "D"
  show (File 4) = "E"
  show (File 5) = "F"
  show (File 6) = "G"
  show (File 7) = "H"
  show _        = "?" -- Should never happen, due to use of smart constructors.

instance Show Rank where
  show (Rank r) = show (r + 1)

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

instance Enum Position where
  fromEnum (Position (File file) (Rank rank)) = rank * 16 + file
  {-# INLINE fromEnum #-}

  toEnum idx = Position (File (idx .&. 7)) (Rank (idx `shiftR` 4))
  {-# INLINE toEnum #-}
