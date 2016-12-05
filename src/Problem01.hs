{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Problem01 (
    partA, partB
    ) where

import Data.Monoid
import Data.List
import Text.Megaparsec
import Text.Megaparsec.String

inputALocation :: FilePath
inputALocation = "input/input01a.txt"

data Rotation = Clockwise | AntiClockwise
    deriving (Eq,Show)

data Move = Move Rotation Int
    deriving (Eq,Show)

data Compass = North | East | South | West
    deriving (Eq,Show,Enum)

data Position = Position {
        posX :: Int
      , posY :: Int
                         } deriving (Eq,Show)
instance Monoid Position where
    mempty = Position 0 0
    Position ax ay `mappend` Position bx by = Position
        (ax + bx) (ay + by)

rotateCompass :: Rotation -> Compass -> Compass
rotateCompass Clockwise North     = East
rotateCompass Clockwise East      = South
rotateCompass Clockwise South     = West
rotateCompass Clockwise West      = North
rotateCompass AntiClockwise North = West
rotateCompass AntiClockwise East  = North
rotateCompass AntiClockwise South = East
rotateCompass AntiClockwise West  = South

compassMove :: Compass -> Int -> Position
compassMove North n = Position 0 (-n)
compassMove East n = Position n 0
compassMove South n = Position 0 n
compassMove West n = Position (-n) 0

applyMove :: Compass -> Position -> Move -> (Compass, Position)
applyMove facing pos (Move rot n) = (newFacing, newPos)
    where newFacing = rotateCompass rot facing
          newPos    = pos <> compassMove newFacing n

finalPosition :: [Move] -> (Compass,Position)
finalPosition = foldl (uncurry applyMove) (North, Position 0 0)

distance :: Position -> Int
distance (Position x y) = abs x + abs y

parseMoves :: Parser [Move]
parseMoves = single `sepBy` string ", "
    where single = Move 
            <$> ((Clockwise <$ char 'R') <|> (AntiClockwise <$ char 'L'))
            <*> (read <$> many digitChar)

partAString :: String -> Either (ParseError Char Dec) Int
partAString = fmap (distance . snd . finalPosition) . runParser parseMoves ""

partA :: IO ()
partA = readFile inputALocation >>= print . partAString

partB :: IO ()
partB = readFile inputALocation >>= print . partBString

partBString :: String -> Either (ParseError Char Dec) (Maybe Int)
partBString = fmap (fmap distance . firstDuplicate . map snd . applyManyMoveSteps) . runParser parseMoves ""

applyMoveSteps :: Compass -> Position -> Move -> [(Compass, Position)]
applyMoveSteps comp pos (Move r n) = map ((newComp,) . mappend pos . compassMove newComp) [1 .. n]
    where newComp = rotateCompass r comp

applyManyMoveSteps :: [Move] -> [(Compass,Position)]
applyManyMoveSteps = helper North mempty
    where helper comp pos [] = []
          helper comp pos (m:ms) = ls ++ helper (fst $ last ls) (snd $ last ls) ms
            where ls = applyMoveSteps comp pos m

firstDuplicate :: Eq a => [a] -> Maybe a
firstDuplicate [] = Nothing
firstDuplicate (m : ms)
    | m `elem` ms = Just m
    | otherwise = firstDuplicate ms
