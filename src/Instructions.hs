module Instructions (
    PInstruction (..)
) where

import qualified Data.DirGraph as G
import CodelType

data PInstruction
    = PPush Int
    | PPop
    | PAdd
    | PSubstract
    | PMultiply
    | PDivide
    | PMod
    | PNot
    | PGreater
    | PPointer
    | PSwitch
    | PDuplicate
    | PRoll
    | PInNum
    | POutNum
    | PInChar
    | POutChar
    | PNop -- eigen toegevoegd
    | PStop -- eigen toegevoegd
    | PSetDP DP -- eigen toegevoegd
    | PSetCC CC -- eigen toegevoegd

