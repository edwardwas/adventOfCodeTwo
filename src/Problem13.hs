{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Problem13 where

import Linear
import Control.Lens
import qualified Data.Map as M
import Data.Maybe

data Space = Wall | Open | Step
    deriving (Eq,Show)

spaceChar Wall = '#'
spaceChar Open = '.'
spaceChar Step = 'O'

newtype Office = Office {_getOffice :: M.Map (V2 Integer) Space}
    deriving (Eq)
makeLenses ''Office

officeWidth :: Office -> Integer
officeWidth = fromMaybe 0 . maximumOf (getOffice . itraversed . asIndex . _x)

officeHeight :: Office -> Integer
officeHeight = fromMaybe 0 . maximumOf (getOffice . itraversed . asIndex . _y)

favourtieNumber :: Integral a => a
favourtieNumber = 10

binaryRepresentation :: Integral a => a -> [a]
binaryRepresentation = map (`mod` 2) . takeWhile (> 0) . iterate (`div` 2)

isWall :: Integral a => a -> a -> Bool
isWall x y = not $ even $ sum $ binaryRepresentation $ 
    favourtieNumber + x*x + 3*x + 2 * x * y + y + y*y

makeOffice w h = Office $ M.fromList $ helper <$> [0..w-1] <*> [0..h-1]
    where helper x y = (V2 x y, if isWall x y then Wall else Open)
