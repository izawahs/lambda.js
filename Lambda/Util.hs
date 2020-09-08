module Lambda.Util (
    foldError, (+++), listTo2Tuple
) where

{-
trim :: String -> String
trim [] = []
trim (' ':cs) = trim cs
trim cs = cs
-}

foldError :: [Either String a] -> [Either String a]
foldError []        = []
foldError [Left e]  = [Left e]
foldError [Right a] = [Right a]
foldError ((Left e1):(Left e2):es) = Left e1 : Left e2 : foldError es
foldError ((Left e):(Right _):es)  = Left e : foldError es
foldError ((Right _):(Left e):es)  = Left e : foldError es
foldError ((Right a):(Right b):es) = Right a : Right b : foldError es

-- | `concat` function for Maybe list.
--   concatenate Nothing as `[]`.
(+++) :: Maybe [a] -> Maybe [a] -> Maybe [a]
Nothing +++ Nothing     = Nothing
(Just ma) +++ Nothing   = Just ma
Nothing +++ (Just mb)   = Just mb
(Just ma) +++ (Just mb) = Just (ma ++ mb)

listTo2Tuple :: [a] -> (a, a)
listTo2Tuple []      = errorWithoutStackTrace "empty list"
listTo2Tuple (x:y:_) = (x, y)

