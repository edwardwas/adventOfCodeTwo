{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Problem10 (partA, partB) where

import Control.Lens
import Data.List
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Text.Megaparsec.Prim
import qualified Data.IntMap as I
import qualified Text.Megaparsec.Lexer as L

inputLocation = "input/input10.txt"

partB = undefined

type Chip = Int

data Bot = Bot (Maybe Chip) (Maybe Chip)
    deriving (Show,Eq)

botLowHigh :: Bot -> Maybe (Chip,Chip)
botLowHigh (Bot (Just x) (Just y)) = Just (min x y, max x y)
botLowHigh _ = Nothing

giveBotChip c (Bot Nothing x) = Bot (Just c) x
giveBotChip c (Bot (Just a) Nothing) = Bot (Just a) (Just c)

data World = World {
      _bots :: I.IntMap Bot
    , _outputBins :: I.IntMap [Chip]
                   } deriving (Eq,Show)
makeLenses ''World

data Target = BotTarget Int | BinTarget Int
    deriving (Eq,Show)

data Move = Move Int Target Target
    deriving (Eq,Show)

addToTarget :: Target -> Chip -> World -> World
addToTarget (BotTarget n) c = bots . at n . non (Bot Nothing Nothing) %~ giveBotChip c
addToTarget (BinTarget n) c = outputBins . at n . non [] %~ (c :)

doMove (Move start t1 t2) w = case preview (bots . ix start) w >>= botLowHigh of
                         Nothing -> w
                         Just (l,h) -> w & addToTarget t1 l & addToTarget t2 h

parseTarget :: (MonadParsec Dec String m) => m Target
parseTarget = botTarget <|> binTarget
    where botTarget = BotTarget <$> (string "bot "    >> (fromIntegral <$> L.integer))
          binTarget = BinTarget <$> (string "output " >> (fromIntegral <$> L.integer))

parseLine :: (MonadParsec Dec String m, MonadState World m) => m [Move]
parseLine = (pure <$> parseMove) <|> ([] <$ parseInput)
    where parseMove = do
            string "bot "
            start <- fromIntegral <$> L.integer
            string " gives low to "
            tLow <- parseTarget
            string " and high to "
            tHigh <- parseTarget
            return $ Move start tLow tHigh
          parseInput = do
              string "value "
              c <- fromIntegral <$> L.integer
              string " goes to "
              t <- parseTarget
              modify $ addToTarget t c

parseInput :: String -> Either (ParseError Char Dec) ([Move], World)
parseInput str = helper $ 
    runState (runParserT (concat <$> many (space *> parseLine <* space)) "" str) (World I.empty I.empty)
        where helper (a,b) = (,b) <$> a
              helper :: Functor f => (f a, b) -> f (a,b)

findChip :: Chip -> World -> Maybe Int
findChip c w = fst <$> w ^@? bots . itraversed . filtered helper
    where helper (Bot c1 c2) = c1 == Just c || c2 == Just c

partA = do
    let helper m w = do
            let w' = foldl' (flip doMove) w m
                Just loc61 = findChip 61 w'
                Just loc17 = findChip 17 w'
            print (loc61, loc17)
            if loc61 == loc17 then return () else helper m w'

    Right (move,w) <- parseInput <$> readFile inputLocation
    helper move w
