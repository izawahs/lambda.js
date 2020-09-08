module Lambda.Parser.Types where

data JSNull = Null
    deriving (Eq)

data JSBool = JSFalse | JSTrue
    deriving (Eq)

data Statement = Var String String
               | Const String String
               | Assign String String

-- TODO: Expression datatype

data Result a = Failure String | Value a

instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Value a)     = Value (f a)
