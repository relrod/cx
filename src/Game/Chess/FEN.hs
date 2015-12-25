{-# LANGUAGE OverloadedStrings #-}

-- | A simple FEN parser using attoparsec.
module Game.Chess.FEN where

import Control.Applicative
import Data.Attoparsec.Text as Atto
import Data.Char (digitToInt, isDigit, isLower, isUpper, toLower)
import Data.List (elem, intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Vector as V
import Game.Chess.Types

charToCells :: Char -> [Cell]
charToCells c
  | isDigit c = replicate (digitToInt c) Empty
  | isLower c = [cellify (toLower c) Black]
  | isUpper c = [cellify (toLower c) White]
  | otherwise = []
  where
    cellify 'p' = Cell Pawn
    cellify 'r' = Cell Rook
    cellify 'n' = Cell Knight
    cellify 'k' = Cell King
    cellify 'q' = Cell Queen
    cellify 'b' = Cell Bishop

parseCell :: Parser Char
parseCell =
  choice $ map char "12345678" ++ map char "PNBRQKpnbrqk"
{-# INLINE parseCell #-}

parseRanks :: Parser [Cell]
parseRanks = do
  ranks <- many1 parseCell `sepBy1` char '/'
  let parsedRanks = map (concatMap charToCells) (reverse ranks)
  return $ intercalate (replicate 8 Empty) parsedRanks ++
    replicate 8 Empty

parseActiveColor :: Parser Color
parseActiveColor = do
  color <- choice [char 'b', char 'w']
  if color == 'b'
    then return Black
    else return White

parseCastleAbility :: Parser [CastleAbility]
parseCastleAbility = do
  castleLetters <- many1 . satisfy $ inClass "kqKQ-"
  -- What is the better way to do this?
  -- This is technically wrong anyway, because we accept things like "KK" and
  -- "K-" which are clearly nonsensical.
  return . catMaybes . handleCastleLetter $ castleLetters
  where
    handleCastleLetter "-" = []
    handleCastleLetter xs = map toCastleAbility xs
    toCastleAbility 'k' = Just $ Kingside Black
    toCastleAbility 'K' = Just $ Kingside White
    toCastleAbility 'q' = Just $ Queenside Black
    toCastleAbility 'Q' = Just $ Queenside White
    toCastleAbility _   = Nothing

-- TODO: Something better than 'Text'.
-- We ideally want something along the lines of 'Cell'.
parseEnPassant :: Parser (Maybe T.Text)
parseEnPassant = do
  enPassant <- string "-" <|> Atto.take 2
  if enPassant == "-"
    then return Nothing
    else return $ Just enPassant

parseHalfmoves :: Parser Integer
parseHalfmoves = read <$> many1 digit

parseFullmoves :: Parser Integer
parseFullmoves = read <$> many1 digit

parseFEN :: Parser Board
parseFEN = do
  ranks <- V.fromList <$> parseRanks
  _ <- char ' '
  activeColor <- parseActiveColor
  _ <- char ' '
  castling <- parseCastleAbility
  _ <- char ' '
  enPassant' <- fmap T.unpack <$> parseEnPassant
  _ <- char ' '
  halfmoves' <- parseHalfmoves
  _ <- char ' '
  fullmoves' <- parseFullmoves
  return $ Board ranks activeColor castling enPassant' halfmoves' fullmoves'
