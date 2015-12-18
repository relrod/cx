module Game.Chess.Board where

import Data.Bits
import Data.List (intersperse)
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

safeBoardIndex :: Board -> Int -> Int -> Maybe Cell
safeBoardIndex b file rank
  | isOnBoard idx = board b V.!? idx
  | otherwise = Nothing
  where
    idx = index file rank

initialBoard :: Board
initialBoard =
  Board
  (V.fromList generateBoard)
  White
  where
    generateBackRank c =
      fmap
      (\p -> Cell p c)
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
  concat .
  intersperse ["|\n|"] .
  reverse .
  map (map show) .
  dropEvery 2 .
  chunksOf 8 $ x
  where
    dropEvery _ [] = []
    dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)
