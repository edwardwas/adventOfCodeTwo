{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Problem04 (partA, partB) where
    
import Data.List
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import qualified Data.Map as M
import qualified Text.Megaparsec.Lexer as L

partA = readFile inputLocation >>= print . partAString
partB = readFile inputLocation >>= print . partBString

inputLocation = "input/input4.txt"

data RoomID = RoomID {
    name :: String
  , sectorId :: Integer
  , checksum ::  String
                     }
    deriving (Eq,Show)

parseRoomID :: Parser RoomID
parseRoomID = do
    name <- concat <$> many ((++) <$> (many lowerChar) <*> string "-")
    sectorID <- L.integer
    char '['
    checksum <- manyTill lowerChar $ char ']'
    return $ RoomID name sectorID checksum

mostCommon :: (Ord a) => Int -> [a] -> [a]
mostCommon n = map fst . take n . reverse . sortBy sortHelper . M.toList . 
    foldr (M.unionWith (+)) M.empty . map (\c -> M.singleton c (1 :: Int))
        where sortHelper (c1,n1) (c2,n2)
                | n1 == n2 = compare c2 c1
                | otherwise = compare n1 n2

validRoomID :: RoomID -> Bool
validRoomID RoomID{..} = checksum == mostCommon 5 (filter (/= '-') name)

partAString :: String -> Either (ParseError Char Dec) Integer
partAString = fmap (sum . map sectorId . filter validRoomID) . 
    parse (many $ parseRoomID <* space) ""

applyShift :: Int -> String -> String
applyShift n = head . drop n . iterate (map helper)
    where helper '-' = '_'
          helper 'z' = 'a'
          helper  x  = succ x

partBString :: String -> Either (ParseError Char Dec) Integer
partBString = fmap (sectorId . head . filter (isInfixOf "pol" . name)
        . map (\r -> r { name = applyShift (fromIntegral $ sectorId r) (name r)})
        . filter validRoomID) .
    parse (many $ parseRoomID <* space) ""
