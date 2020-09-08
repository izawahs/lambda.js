module Lambda.Parser.Number (
    parseInt, parseSInt
) where

import Control.Applicative ((<|>))
import Lambda.Util ((+++), listTo2Tuple)
import Data.Maybe (fromJust)
import Data.List.Split
import Data.List (foldl1)

-- | Take a Int value `i` and a string, and return `Just i` if the string is the same as `show i`, otherwise `Nothing`.
digit :: Int -> String -> Maybe Int
digit _ [] = Nothing
digit i (c:_) | i > 9 || i < 0 = Nothing
              | otherwise =
                if [c] == show i
                    then Just i
                    else Nothing

digits :: [String] -> [Maybe String]
digits = map (fmap show . parseInt)

parseInt :: String -> Maybe Int
parseInt s = foldl1 (<|>) $ map (flip digit $ s) [0..9]

parseSInt :: String -> Either String String
parseSInt s =
    let parsed = foldr1 (+++) $ (digits . strToList) s
    in if parsed == Nothing
        then Left "illegal integer"
        else Right $ fromJust parsed
  where strToList = map (:[])


parseSFloat :: String -> Either String String
parseSFloat fs =
    if '.' `notElem` fs
        then Left "illegal float"
        else case checkThenWrap fs of
            (Left _, Left _)   -> Left "illegal float"
            (Right _, Right _) -> Right fs
    where checkThenWrap s = listTo2Tuple $ map parseSInt $ splitOn "." s


parseFloat :: String -> Maybe Float
parseFloat fs =
    case parseSFloat fs of
        Left err -> Nothing
        Right s  -> Just (read s)
