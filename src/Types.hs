module Types
        ( Index
        , Marker (..)
        , Direction
        ) where

type Index = (Int, Int)
type Direction = Index

data Marker = Empty | X | O deriving (Show, Eq)

