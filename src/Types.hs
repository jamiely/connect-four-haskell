module Types
        ( Index
        , Marker (..)
        ) where

type Index = (Int, Int)

data Marker = Empty | X | O deriving (Show, Eq)

