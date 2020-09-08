module Lambda.Parser.Constants (
    parseNull, parseBool
) where

import Lambda.Types

parseNull :: String -> Either String JSNull
parseNull "null" = Right Null
parseNull _      = Left "parse failed"

parseBool :: String -> Either String JSBool
parseBool "true"  = Right JSTrue
parseBool "false" = Right JSFalse
parseBool _       = Left "parse failed"
