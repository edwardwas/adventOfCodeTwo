{-# LANGUAGE OverloadedStrings #-}

module Problem14 (partA, partB) where

import Data.Maybe
import Data.Monoid
import Control.Monad
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

iterHash :: Int -> B8.ByteString -> B8.ByteString
iterHash n = last . take (n + 1) . iterate (encode . MD5.hash)

generateHashes :: Int -> [(Int,String)]
generateHashes n = zip [0..] $ map (tail . init . show . iterHash n . (salt <>) . 
        B8.pack . show) [0..]
    where salt = "cuanljph"

getRepeatingPattern :: Eq a => Int -> [a] -> Maybe a
getRepeatingPattern n (x : xs)
  | all (== x) (take (n-1) xs) && length xs >= (n-1) = Just x
  | otherwise = getRepeatingPattern n xs
getRepeatingPattern n [] = Nothing

genKeys :: Int -> [Int]
genKeys n = mapMaybe helper $ iterate tail $ generateHashes n
    where helper ((n,x):xs) = do
            patNum <- getRepeatingPattern 3 x
            guard $ any (== (Just patNum)) $ map (getRepeatingPattern 5 . snd ) 
                $ take 1000 xs
            return n

partA = print $ genKeys 1 !! 63
partB = print $ genKeys 2017 !! 63

