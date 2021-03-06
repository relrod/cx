{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
module Game.Chess.Types (
    Board (..)
  , CastleAbility (..)
  , MovingVector (..)
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
  , MoveValidity (..)
  , parsePosition

  -- * Smart constructors
  , mkFile
  , mkRank
  , mkPosition

  -- * Utility functions
  , mkMovingVector
  , mkTuple
) where

import Data.Bits
import Data.Char (toUpper)
import qualified Data.Vector as V
import Text.Read (readMaybe)

-- I am torn on using a bitboard (or any piece-centric board representation) as
-- opposed to a square-centric representation which seems more intuitive to me.
-- https://chessprogramming.wikispaces.com/Board+Representation
-- So, this might change. Edward Kmett has some thoughts on a bitboard
-- representation here: https://da.gd/RQeJK
--
-- For now, I am using a 0x88 representation, which is still more low-level
-- than I am used to, but nevertheless, should be simple enough to work with.

-- | 'Board' represents the current state of the game at any given moment.
data Board =
  Board { board :: V.Vector Cell
        , sideToMove :: Color
        , castleAbility :: [CastleAbility]
        , enPassant :: Maybe String -- TODO: 'String' is bad
        , halfmoves :: Integer
        , fullmoves :: Integer
        } deriving (Eq, Ord, Show)

-- | Who can castle and on which side(s)?
data CastleAbility =
    Kingside Color
  | Queenside Color
  deriving (Eq, Ord, Show)

-- | There are 6 different kinds of chess pieces.
data Piece =
    Pawn
  | Knight
  | Bishop
  | King
  | Queen
  | Rook
  deriving (Eq, Ord, Show, Enum)

-- | Each piece is 'White' or 'Black'.
data Color = White | Black deriving (Eq, Ord, Show, Enum)

-- | A 'Cell' represents a square on the board. It is either 'Empty' or a 'Cell'
-- containing a particular 'Piece' which belongs to a 'Color'.
--
-- This is isomorphic to 'Maybe' ('Piece', 'Color').
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

-- | During move generation, we need a way to describe the validity of moves.
--
-- Eventually we will need a @SameSideCheck@ constructor to indicate that the
-- given move would cause the current side\'s king to be placed in check.
data MoveValidity =
    EmptySquare -- ^ We are moving to an empty cell.
  | Take -- ^ We are moving to a cell that has an opposite-color piece.
  | Occupied -- ^ We are moving to a cell that has a same-color piece.
  | InvalidPawnCapture -- ^ We are a 'Pawn' wanting to capture what we cannot.
  | StartFromEmptySquare -- ^ We tried to move a piece from an empty square.
  | KingCapture -- ^ We tried to capture a king.
  | WrongColorMove -- ^ We tried to move a piece that is not ours.
  deriving (Eq, Ord, Show)

-- | There are certain cases where a moving vector for a piece is more than
-- just a 2-tuple position vector. The most obvious case is the set of moving
-- vectors for 'Pawn's. Pawns capture diagonally, so we need a way to denote
-- \"this vector only applies when capturing.\"
data MovingVector a =
    NormalMove a a
  | PawnCaptureMove a a
  deriving (Eq, Functor, Ord, Show)

-- | A helper function for converting from regular 2-tuples to 'MovingVector's.
--
-- Should be called with something like @'mkMovingVector' (1, 1) 'NormalMove'@.
mkMovingVector :: (a -> a -> MovingVector a) -> (a, a) -> MovingVector a
mkMovingVector f (a, b) = f a b

-- | This goes the other way, converting a @'MovingVector' a@ to @(a, a)@.
--
-- It is only here until the Position module gets updated to account for the
-- addition of the 'MovingVector' type. Do not rely on it.
mkTuple :: MovingVector a -> (a, a)
mkTuple (NormalMove a b) = (a, b)
mkTuple (PawnCaptureMove a b) = (a, b)

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

-- | A helper to parse positions from a 'String'.
parsePosition :: String -> Maybe Position
parsePosition [f, r] =
  case readMaybe [r] :: Maybe Int of
    Just rank ->
      mkPosition (letterToFile (toUpper f) >>= mkFile) (mkRank (rank - 1))
    Nothing -> Nothing
  where
    letterToFile 'A' = Just 0
    letterToFile 'B' = Just 1
    letterToFile 'C' = Just 2
    letterToFile 'D' = Just 3
    letterToFile 'E' = Just 4
    letterToFile 'F' = Just 5
    letterToFile 'G' = Just 6
    letterToFile 'H' = Just 7
    letterToFile _ = Nothing
parsePosition _ = Nothing

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
