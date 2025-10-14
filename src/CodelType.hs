module CodelType (
    DP (..),
    CC (..),
    nextseq,
    rotDP,
    toggleCC
) where


data DP = N | E | S | W deriving (Eq, Show, Enum, Ord)
data CC = CCLeft | CCRight deriving (Eq, Show, Ord)

nextseq :: (DP, CC) -> [(DP, CC)]
nextseq (d,c) = take 8 $ (d,c) : (d, toggleCC c) : nextseq (rotDP CCRight d, toggleCC c)

rotDP :: CC -> DP -> DP
rotDP CCLeft = toEnum . (`rem` 4) . (+4) . pred . fromEnum
rotDP CCRight = toEnum . (`rem` 4) . (+4) . succ . fromEnum

toggleCC :: CC -> CC
toggleCC CCLeft = CCRight
toggleCC CCRight = CCLeft