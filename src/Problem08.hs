{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Problem08 (partA, partB) where

import Data.List
import Control.Lens
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

inputLocation = "input/input8.txt"

transposeIso :: Iso' [[a]] [[a]]
transposeIso = iso transpose transpose

data ScreenAction = TurnOn Int Int
    | RotateRow Int
    | RotateColumn Int
        deriving (Eq,Show)

newtype Screen = Screen {getScreen :: [[Bool]]}
    deriving (Eq)

lightsOn (Screen s) = sum $ map (length . filter id) s

emptyScreen :: Int -> Int -> Screen
emptyScreen w h = Screen $ replicate h $ replicate w False

instance Show Screen where
    show (Screen a) = unlines $ map (map (\x -> if x then '#' else '.')) a

instance Wrapped Screen where
    type Unwrapped Screen = [[Bool]]
    _Wrapped' = iso getScreen Screen
instance (t ~ Screen) => Rewrapped Screen t

applyAction :: Screen -> ScreenAction -> Screen
applyAction scr (TurnOn x y) = scr & _Wrapped . ix y . ix x .~ True
applyAction scr (RotateRow y) = scr & _Wrapped . ix y %~ helper
    where helper xs = last xs : init xs
applyAction scr (RotateColumn y) = scr & _Wrapped . transposeIso . ix y %~ helper
    where helper xs = last xs : init xs

applyManyAction :: Screen -> [ScreenAction] -> Screen
applyManyAction = foldl' applyAction 

rect :: Int -> Int -> [ScreenAction]
rect w h = TurnOn <$> [0..w-1] <*> [0..h-1]

rotateColumnBy :: Int -> Int -> [ScreenAction]
rotateColumnBy x n = replicate n $ RotateColumn x

rotateRowBy :: Int -> Int -> [ScreenAction]
rotateRowBy y n = replicate n $ RotateRow y

parseInt :: Parser Int
parseInt = fromIntegral <$> L.integer

parseActions :: Parser [ScreenAction]
parseActions = (rect <$> (string "rect " *> parseInt) 
            <*> (char 'x' *> parseInt))
    <|> (rotateColumnBy <$> (string "rotate column x=" *> parseInt) 
            <*> (string " by " *> parseInt))
    <|> (rotateRowBy <$> (string "rotate row y=" *> parseInt)
            <*> (string " by " *> parseInt))

partA :: IO ()
partA = do
    lines <- lines <$> readFile inputLocation
    let instructions = concat $ either (error . show) id $
            mapM (parse parseActions "") lines
    let screenOutput = applyManyAction (emptyScreen 50 6) instructions
    print $ lightsOn screenOutput

partB :: IO ()
partB = do
    lines <- lines <$> readFile inputLocation
    let instructions = concat $ either (error . show) id $
            mapM (parse parseActions "") lines
    print $ applyManyAction (emptyScreen 50 6) instructions
