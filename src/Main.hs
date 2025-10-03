module Main (main) where

import Codel (codelsFromImage, codelMapToGraph)



import qualified Data.Set as S
import qualified Data.DirGraph as G


main :: IO ()
main = do
    ima <- codelsFromImage "./tests/Piet_hello_small.png" -- "./tests/Piet_hello_small.png"
    --print ima
    case ima of
        Left err -> putStrLn err
        Right c@(codelm, codels) -> do
            print codelm
            putStrLn ""
            putStrLn ""
            print codels
            putStrLn ""
            putStrLn ""
            print (S.size codels) 
            putStrLn "" 
            let (g,start) = codelMapToGraph c
                g' = G.mapArcs (\v1 v2 (dp,cc, _) -> (show v1 ++ show v2 ++ show dp ++ show cc)) g
            print g
            print g'
            print start