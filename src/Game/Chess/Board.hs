module Game.Chess.Board where

import Data.Bits
import Data.List (intersperse)
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
    generateEmptyRank = replicate 8 (Cell None White)
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

prettyPrintBoard :: [Cell] -> String -> String
prettyPrintBoard [] acc = init acc
prettyPrintBoard cs acc =
  prettyPrintBoard
    (drop 16 cs) -- Note: 16 because of 0x88 board instead of 8x8.
    (acc ++ (concat . intersperse " " . map show . take 16 $ cs) ++ "\n")
