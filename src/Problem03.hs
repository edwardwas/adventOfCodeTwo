{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Problem03 (partA, partB) where

import Data.List
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Control.Monad.State
import Control.Lens
import qualified Text.Megaparsec.Lexer as L

inputLocaion :: FilePath
inputLocaion = "input/input3.txt"

partA = readFile inputLocaion >>= print . partAString
partB = readFile inputLocaion >>= print . partBString

data Triangle = Triangle Integer Integer Integer
    deriving (Eq,Show)

parseTriangle :: Parser Triangle
parseTriangle = space >> Triangle
    <$> L.integer
    <* space
    <*> L.integer
    <* space
    <*> L.integer

readTriangles :: String -> Maybe [Triangle]
readTriangles = either (const Nothing) Just . parse (parseTriangle `sepEndBy` eol) ""

isValidTriangle :: Triangle -> Bool
isValidTriangle (Triangle a b c) = a < b + c
    && b < a + c
    && c < a + b

partAString :: String -> Maybe Int
partAString = fmap (length . filter isValidTriangle) . readTriangles

parseTriangleB :: String -> [Triangle]
parseTriangleB input = makeTriangle $ a ++ b ++ c
    where helper = parseLine `sepEndBy` eol
          (a,b,c) = execState (runParserT helper "" input) ([],[],[])
          makeTriangle [] = []
          makeTriangle (x : y : z : xs) = Triangle x y z : makeTriangle xs
          parseLine :: ParsecT Dec String (State ([Integer],[Integer],[Integer])) ()
          parseLine = do
              a <- space *> L.integer 
              b <- space *> L.integer
              c <- space *> L.integer
              _1 %= (a:)
              _2 %= (b:)
              _3 %= (c:)

partBString :: String -> Int
partBString = length . filter isValidTriangle . parseTriangleB
