{-# LANGUAGE ScopedTypeVariables #-}

module Problem09 (partA, partB) where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Control.Applicative
import Control.Monad
import Debug.Trace

partA = undefined
partB = undefined

inputLocation :: FilePath
inputLocation = "input/input9.txt"

stripWhiteSpace = filter (\x -> not $ x `elem` ['\n', ' '])

decompres :: Parser String
decompres = ([] <$ eof) <|> singleParse <|> helper
    where singleParse = (:) <$> upperChar <*> decompres
          helper = do
              char '('
              a <- fromIntegral <$> L.integer
              char 'x' 
              b <- fromIntegral <$> L.integer
              char ')'
              nextChars <- count a anyChar
              (concat (replicate b nextChars) ++) <$> decompres

partAString :: String -> Either (ParseError Char Dec) Int
partAString = fmap length . parse decompres "" . stripWhiteSpace

-- PART B ----

data Marker = Rep Int Int | Sym Char Int
    deriving (Eq,Show)

numDigits :: Int -> Int
numDigits = length . show

decompressBN  :: Int -> Parser Int
decompressBN 0 = return 0
decompressBN n = singleParse <|> helper
    where singleParse = (+) <$> (1 <$ upperChar) <*> decompressBN (n-1)
          helper = do
              char '('
              a <- fromIntegral <$> L.integer
              char 'x' 
              b <- fromIntegral <$> L.integer
              char ')'
              let digitsTake = 3 + numDigits a + numDigits b
              x <- (b * ) <$> decompressBN a
              next <- decompressBN (n - digitsTake - a)
              return $ x + next

partBString str = parse (decompressBN $ length str) "" str

