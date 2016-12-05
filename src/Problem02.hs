module Problem02 (partA, partB) where

import Data.Monoid
import Text.Megaparsec
import Text.Megaparsec.String

inputLocation :: FilePath
inputLocation = "input/input02.txt"

data Offset = Negative | Neutral | Positive
    deriving (Eq,Show,Enum,Ord)

instance Monoid Offset where
    mempty = Neutral
    mappend Negative Positive = Neutral
    mappend Negative _        = Negative
    mappend Positive Negative = Neutral
    mappend Positive _        = Positive
    mappend Neutral  a        = a

data Position = Position Offset Offset
    deriving (Eq,Show)

instance Monoid Position where
    mempty = Position mempty mempty
    Position ax ay `mappend` Position bx by = Position (ax <> bx) (ay <> by)

positionToNumber :: Position -> Int
positionToNumber (Position Negative Negative) = 1
positionToNumber (Position Neutral  Negative) = 2
positionToNumber (Position Positive Negative) = 3
positionToNumber (Position Negative Neutral ) = 4
positionToNumber (Position Neutral  Neutral ) = 5
positionToNumber (Position Positive Neutral ) = 6
positionToNumber (Position Negative Positive) = 7
positionToNumber (Position Neutral  Positive) = 8
positionToNumber (Position Positive Positive) = 9

parseInstruction :: Parser [[Position]]
parseInstruction = helper `sepBy` eol
    where helper = many $ (Position Neutral Negative <$ char 'U')
            <|> (Position Neutral Positive <$ char 'D')
            <|> (Position Positive Neutral <$ char 'R')
            <|> (Position Negative Neutral <$ char 'L')

partA :: IO ()
partA = readFile inputLocation >>= print . fmap (foldMap show . runA) . runParser parseInstruction ""

runA :: [[Position]] -> [Int]
runA = map positionToNumber . helper mempty . filter (not . null)
    where helper _ [] = []
          helper s (x : xs) = newS : helper newS xs
            where newS = foldl mappend s x

data PositionB = PositionB Int Int
    deriving (Eq,Show)

isValidGrid :: PositionB -> Bool
isValidGrid (PositionB x y) = abs x <= 2 && abs y <= helper (abs x)
    where helper 0 = 2
          helper 1 = 1
          helper 2 = 0

movePosition :: PositionB -> (Int,Int) -> PositionB
movePosition p@(PositionB sx sy) (dx,dy) = if isValidGrid newPos then newPos else p
    where newPos = PositionB (sx + dx) (sy + dy)

parseBInstructions :: Parser [[(Int,Int)]]
parseBInstructions = helper `sepBy` eol
    where helper = many $ ((0,-1) <$ char 'U')
            <|> ((0,1 ) <$ char 'D')
            <|> ((1,0)  <$ char 'R')
            <|> ((-1,0) <$ char 'L')

gridBToNum :: PositionB -> Char
gridBToNum (PositionB 0 (-2)) = '1'
gridBToNum (PositionB (-1) (-1)) = '2'
gridBToNum (PositionB 0 (-1)) = '3'
gridBToNum (PositionB 1 (-1)) = '4'
gridBToNum (PositionB (-2) 0) = '5'
gridBToNum (PositionB (-1) 0) = '6'
gridBToNum (PositionB 0 0) = '7'
gridBToNum (PositionB 1 0) = '8'
gridBToNum (PositionB 2 0) = '9'
gridBToNum (PositionB (-1) 1) = 'A'
gridBToNum (PositionB 0 1) = 'B'
gridBToNum (PositionB 1 1) = 'C'
gridBToNum (PositionB 0 2) = 'D'
gridBToNum (PositionB x y) = error $ "Grid to num: " ++ show x ++ " " ++ show y

runB :: [[(Int,Int)]] -> [Char]
runB = map gridBToNum . helper (PositionB (-2) 0) . filter (not . null)
    where helper _ [] = []
          helper s (x : xs) = newS : helper newS xs
            where newS = foldl movePosition s x

partB :: IO ()
partB = readFile inputLocation >>= print . fmap runB . runParser parseBInstructions ""
