module CodelType (
    DP (..),
    CC (..)
) where


data DP = N | E | S | W deriving (Eq, Show, Enum, Ord)
data CC = CCLeft | CCRight deriving (Eq, Show, Ord)