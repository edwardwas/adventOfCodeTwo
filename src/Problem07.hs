{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Problem07 (partA, partB) where

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Monoid
import Data.Maybe

partA = partAString <$> readFile inputLocation
partB = partBString <$> readFile inputLocation

inputLocation = "input/input7.txt"

data IP = IP {
      notHyper :: [String]
    , hyper :: [String]
             }
    deriving (Eq,Show)

mkNotHyper s = IP [s] []
mkHyper s = IP [] [s]

instance Monoid IP where
    mempty = IP [] []
    mappend (IP a b) (IP c d) = IP (a <> c) (b <> d)

parseIP :: Parser IP
parseIP = helper mempty 
    where helper s = do
            a <- fmap mkNotHyper <$> optional (many lowerChar)
            case a of 
              Nothing -> return s
              Just x -> do
                  b <- fmap mkHyper <$> optional (between (char '[') (char ']') (many lowerChar))
                  case b of
                    Nothing -> return $ s <> x
                    Just y -> helper (s <> x <> y)

containsABBA :: Eq a => [a] -> Bool
containsABBA (a : b : c : d : xs) = (a == d && b == c && a /= b) || containsABBA (b : c : d : xs)
containsABBA _ = False

getABA :: Eq a => [a] -> [(a,a)]
getABA (a : b : c : xs)
  | (a == c && a /= b)  = (a,b) : getABA (b : c : xs)
  | otherwise = getABA (b : c : xs)
getABA _ = []

flipTuple (a,b) = (b,a)

hasSSL :: IP -> Bool
hasSSL (IP nh h) = or $ (==) <$> aba <*> bab
    where aba = foldMap getABA nh
          bab = map flipTuple $ foldMap getABA h

hasTLS :: IP -> Bool
hasTLS (IP nh h) = any containsABBA nh && not (any containsABBA h)

partAString = fmap (length . filter hasTLS) <$> mapM (parse parseIP "") . lines
partBString = fmap (length . filter hasSSL) <$> mapM (parse parseIP "") . lines

