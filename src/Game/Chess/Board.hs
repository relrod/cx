module Game.Chess.Board where

import Data.Bits
import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)
import Data.Word
import qualified Data.Vector as V
import Game.Chess.Types

isOnBoard :: (Num a, Bits a) => a -> Bool
isOnBoard a = a .&. 0x88 == 0
{-# INLINE isOnBoard #-}

index :: Num a => a -> a -> a
index file rank = rank * 16 + file
{-# INLINE index #-}

rank :: Bits a => a -> a
rank idx = idx `shiftR` 4
{-# INLINE rank #-}

file :: (Num a, Bits a) => a -> a
file idx = idx .&. 7
{-# INLINE file #-}

safeBoardIndex :: Board -> Position -> Maybe Cell
safeBoardIndex b (Position file rank)
  | isOnBoard idx = board b V.!? idx
  | otherwise = Nothing
  where
    idx = index file rank

initialBoard :: Board
initialBoard =
  Board
  (V.fromList generateBoard)
  White
  [Kingside Black, Kingside White, Queenside Black, Queenside White]
  Nothing
  0
  0
  where
    generateBackRank c =
      fmap
      (`Cell` c)
      [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    generatePawnRank c = replicate 8 (Cell Pawn c)
    generateEmptyRank = replicate 8 Empty
    -- This is in order of index, not in actual order.
    generateBoard =
        generateBackRank White ++ generateEmptyRank ++
        generatePawnRank White ++ generateEmptyRank ++
        generateEmptyRank ++ generateEmptyRank ++
        generateEmptyRank ++ generateEmptyRank ++
        generateEmptyRank ++ generateEmptyRank ++
        generateEmptyRank ++ generateEmptyRank ++
        generatePawnRank Black ++ generateEmptyRank ++
        generateBackRank Black ++ generateEmptyRank

-- This is really inefficient for now.
prettyPrintBoard :: [Cell] -> String
prettyPrintBoard cs =
  (++ " |") .
  (" | " ++) .
  intersperse ' ' .
  concat .
  intercalate ["|\n|"] .
  reverse .
  map (map show) .
  dropEvery 2 .
  chunksOf 8 $ cs
  where
    dropEvery _ [] = []
    dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)
