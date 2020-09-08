module Lambda.Parser.Statement where

import Data.List.Split

-- TODO: replace String to Text
type Stmt = String

-- TODO: 'var' statement
-- TODO: 'const' statement
{-
parseAssign :: Stmt -> Either String (String, String)
parseAssign stmt =
    case splitAssign stmt of
        Left err -> Left err
        Right [name, value] ->
            let tokenPair = (name, value) in
                case parseString name of
                    Left invErr -> Left invErr
                    Right name -> Right tokenPair
-}

-- | 'var' statement parser
parseVar :: Stmt -> Either String (String, String)
parseVar stmt = let tokens = tokenize stmt in
    if "=" `elem` tokens && "var" `elem` tokens
    then getNameValue tokens
    else Left "expected '=' and 'var' , but not exist"

getNameValue :: [String] -> Either String (String, String)
getNameValue ("var":name:"=":value:_) = Right (name, value)
getNameValue _ = Left "invalid statement"

tokenize :: String -> [String]
tokenize = words

parseString :: String -> Either String String
parseString s
    | all (`elem` (['a' .. 'z'] ++ ['A' .. 'Z'])) s
         = Right s
    | otherwise = Left $ '\'' : s ++ "' is illegal string"

splitStmts :: String -> [Stmt]
splitStmts = splitOn ";"
