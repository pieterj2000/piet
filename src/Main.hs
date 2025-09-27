module Main (main) where

import Codel (codelsFromImage, codelMapToGraph)



import qualified Data.Set as S


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
            print $ codelMapToGraph c