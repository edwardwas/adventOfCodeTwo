{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Problem05 (partA, partB, partBCine) where

import Control.Applicative
import System.Console.ANSI
import Control.Lens
import Data.ByteString.Base16
import Data.List
import Data.Maybe
import Data.Monoid
import Safe
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

doorId :: B.ByteString
doorId = "ojvtpuvg"

partAWithInput :: B.ByteString -> [Char]
partAWithInput input = take 8 $ mapMaybe (helper . tail . show . encode . hash . B8.pack . show) [0..]
    where helper xs = if take 5 xs == "00000" then Just (xs !! 5) else Nothing
          hash = MD5.finalize . MD5.update hashStart
          hashStart = MD5.update MD5.init input 

newtype Password = Password {getPassword :: [Maybe Char]}
    deriving (Eq)

instance Show Password where
    show = map (fromMaybe '-') . getPassword

instance t ~ Password => Rewrapped Password t
instance Wrapped Password where
    type Unwrapped Password = [Maybe Char]
    _Wrapped' = iso getPassword Password

type instance IxValue Password = Maybe Char
type instance Index Password = Int

instance Ixed Password where
    ix n = _Wrapped' . ix n

instance Monoid Password where
    mempty = Password $ replicate 8 Nothing
    mappend (Password a) (Password b) = Password $ zipWith (<|>) a b

addGuess :: Password -> (Int,Char) -> Password
addGuess pw (n,c)
  | n >= 0 && n < 8 = pw & ix n %~ (<|> (Just c))
  | otherwise = pw

makeHashses :: B.ByteString -> [String]
makeHashses input = map (tail . show . encode . hash . B8.pack . show) [0..]
    where hash = MD5.finalize . MD5.update hashStart
          hashStart = MD5.update MD5.init input 

guesses :: [String] -> [Password]
guesses = scanl guessHelper mempty . map helper
    where helper xs = if take 5 xs == "00000" then (,xs !! 6) <$> readMay [xs !! 5] else Nothing
          guessHelper pw Nothing = pw
          guessHelper pw (Just x) = addGuess pw x

filledPassword :: Password -> Bool
filledPassword = all isJust . getPassword

mixPassword :: Password -> String -> String
mixPassword pw = zipWith (flip fromMaybe) (getPassword pw)

partA = print $ partAWithInput doorId
partB = print $ head $ filter filledPassword $ guesses $ makeHashses doorId

partBCine = mapM_ showHelper $ zipWith mixPassword (guesses g) g
    where g = makeHashses doorId
          showHelper s = clearLine >> putStrLn s  >> cursorUp 1
