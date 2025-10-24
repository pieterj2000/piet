module Main (main) where

import Codel



import qualified Data.Set as S
import qualified Data.DirGraph as G
import Interpreter (runProgram)
import Data.Char (ord)


--TODO: gcd werkt wel, maar de code klopt niet. Hij stopt als in r=m%n uitkomt dat 1<r, maar als r=1 is dan output hij n, ook al is n>1
-- Bijvoorbeeld hij geeft gcd(12,17)=2. 

{-
TESTS

Foute_kleur             -> Error foute kleur
Piet_hello_small        -> "Hello world!"
Piet_hello              -> "Hello world!"
Fibonacci               -> infinite loop
Hanoi                   -> infinite loop, gekke output
Piet_hello2             -> Error foute kleur
Piet_hello2_small       -> Error foute kleur
Priem1_klein            -> Error foute kleur
Priem1                  -> Error foute kleur
Priem2_klein            -> Error foute kleur
Priem2                  -> Error foute kleur
Hello1                  -> "Hello, world!\n"
Hello1_small            -> "Hello, world!\n"
Hello2                  -> infinite loop, wel eerst output "Hello, world!\n"
Hello2_small            -> infinite loop, wel eerst output "Hello, world!\n"
Hello_artistic1         -> "Hello, world!\n"
Hello_artistic1_small   -> "Hello, world!\n"
Hello_artistic2         -> "Hello, world!\n"
Hello_artistic2_small   -> "Hello, world!\n"
Piet_piet_small         -> infinite loop
Piet_piet               -> infinite loop
Alpha                   -> "abcdefghijklmnopqrstuvwxyz"
Alpha_small             -> "abcdefghijklmnopqrstuvwxyz"
Priem3                  -> Error foute kleur
Priem3_small            -> Error foute kleur
Adder                   -> Werkt goed, twee nummers invoeren, krijgt som
Adder_small             -> Werkt goed, twee nummers invoeren, krijgt som
Pi                      -> "31405\n"
Pi_small                -> "31405\n"
Gcd                     -> werkt soort van, zie vorige opmerking
Gcd_small               -> werkt soort van, zie vorige opmerking
Japh                    -> Werkt niet, het zijn geen vierkanten //TODO-> codels mogen ook rechthoeken zijn (?)
Japh_small              -> infinite loop, geen idee wat het moet doen...
Power                   -> Werkt goed, twee nummers invoeren, krijgt a^b
Power_small             -> Werkt goed, twee nummers invoeren, krijgt a^b
Factorial               -> Error foute kleur (Werkt volgens de online ding wel) TODO: een `permissive' mode toevoegen dat foute kleuren worden gelezen
Factorial_small         -> Error foute kleur (Werkt volgens de online ding wel)                      als de dichtbijzijnste andere kleur
99bottles               -> Werkt! Kijk de output maar of hij werkt
99bottles_small         -> Werkt! Kijk de output maar of hij werkt
Hello_mondriaan         -> "Hello, world!\n"
Hello_mondriaan_small   -> "Hello, world!\n"
Priem4                  -> Werkt niet goed, werkt semi-wel, maar output slecht
Priem4_small            -> Werkt niet goed, werkt semi-wel, maar output slecht
Hello_nietpastel        -> infinite loop, wel eerst output "Hello, world!\n"
Hello_nietpastel_small  -> infinite loop, wel eerst output "Hello, world!\n"
Hello_world             -> "Hello, world!\n"
Hello_world_small       -> "Hello, world!\n"



-}

main :: IO ()
main = do
    ima <- codelsFromImage "./tests/Hello_world.png" -- "./tests/Piet_hello_small.png"
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
