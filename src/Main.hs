module Main (main) where

import Codel



import qualified Data.Set as S
import qualified Data.DirGraph as G
import Interpreter (runProgram)
import Data.Char (ord)


main :: IO ()
main = do
    ima <- codelsFromImage "./tests/Gcd.png" -- "./tests/Piet_hello_small.png"
    --print ima
    case ima of
        Left err -> putStrLn err
        Right c@(codelm, codels) -> do
            --print $ sum $ map ord $ show codelm
            --getLine
            print codelm
            --getLine
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
            --print g'
            print start
            putStrLn ""
            putStrLn ""
            let g1 = g --G.splitNodesIndegOne g
                g2 = edgesToInstrNodes g1
                g3 = addStartNode start g2
                --g4 = (uncurry instGraphFromCodelGraph2) g3
                --aaa = (uncurry instGraphFromCodelGraph2) . (addStartNode startnode) . edgesToInstrNodes . splitNodesIndegOne


                gfinal = instGraphFromCodelGraph start g

                (startnode, echtegraaf) = gfinal
            putStrLn ""
            putStrLn ""
            print g1
            putStrLn ""
            putStrLn ""
            print g2
            putStrLn ""
            putStrLn ""
            print g3
            putStrLn ""
            putStrLn ""
            --print g4
            print gfinal
            putStrLn ""
            putStrLn ""
            --print g1
            getLine

            runProgram startnode echtegraaf
