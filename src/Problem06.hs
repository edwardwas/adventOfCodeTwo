{-# LANGUAGE PartialTypeSignatures #-}

module Problem06 (partA, partB) where

import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as M

partA = readFile inputLocation >>= print . partAString
partB = readFile inputLocation >>= print . partBString

inputLocation = "input/input6.txt"

mostCommon :: Ord a => [a] -> a
mostCommon =  fst . maximumBy (compare `on` snd) . M.toList
    . foldl' (M.unionWith (+)) M.empty . map (`M.singleton` (1 :: Int))

leastCommon :: Ord a => [a] -> a
leastCommon =  fst . minimumBy (compare `on` snd) . M.toList
    . foldl' (M.unionWith (+)) M.empty . map (`M.singleton` (1 :: Int))

partAString :: String -> String
partAString = map mostCommon . transpose . lines

partBString :: String -> String
partBString = map leastCommon . transpose . lines
