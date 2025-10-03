{-# LANGUAGE InstanceSigs #-}
module Codel (
    Color,
    Lightness,
    Image,
    codelsFromImage,
    codelMapToGraph,
    instGraphFromCodelGraph1
) where

    
import qualified Data.Array.IArray as A
import qualified Data.Map as M -- TODO Intmap of Array of Vector
import qualified Data.Set as S
import Codec.Picture hiding (Image)
import Data.Function (on)
import Data.List (sortOn, groupBy, (\\))
import Data.Ord (Down (Down))
import Control.Monad ((<=<))
import qualified Data.DirGraph as G
import Data.Maybe (isJust)
import Instructions

import CodelType

import Control.Monad.State

import qualified Debug.Trace
import qualified Debug.Trace as Debug

    
type Image a = A.Array (Int, Int) a
type ImageM a = M.Map (Int, Int) a

data IndirType a = None | ThroughWhite a DP CC | Directly a deriving (Eq, Show, Ord)
fromIndir :: IndirType a -> a
fromIndir None = error "fromIndir None"
fromIndir (ThroughWhite a _ _) = a
fromIndir (Directly a) = a
instance Functor IndirType where
  fmap :: (a -> b) -> IndirType a -> IndirType b
  fmap _ None = None
  fmap f (Directly a) = Directly (f a)
  fmap f (ThroughWhite a dp cc) = ThroughWhite (f a) dp cc

data Codel a = Codel {
    lightness :: Lightness,
    color :: Color,
    numPixelEqs :: Int,
    nextCodel :: M.Map (DP,CC) (IndirType a)
} deriving (Show, Eq, Ord)

data Color = Red | Yellow | Green | Cyan | Blue | Magenta | White | Black deriving (Eq, Enum, Show, Ord)
data Lightness = Light | Normal | Dark deriving (Eq, Enum, Show, Ord)

pixeltocolor :: PixelRGB8 -> Either String (Lightness, Color)
pixeltocolor (PixelRGB8 0xFF 0xC0 0xC0) = Right (Light, Red)
pixeltocolor (PixelRGB8 0xFF 0xFF 0xC0) = Right (Light, Yellow)
pixeltocolor (PixelRGB8 0xC0 0xFF 0xC0) = Right (Light, Green)
pixeltocolor (PixelRGB8 0xC0 0xFF 0xFF) = Right (Light, Cyan)
pixeltocolor (PixelRGB8 0xC0 0xC0 0xFF) = Right (Light, Blue)
pixeltocolor (PixelRGB8 0xFF 0xC0 0xFF) = Right (Light, Magenta)
pixeltocolor (PixelRGB8 0xFF 0x00 0x00) = Right (Normal, Red)
pixeltocolor (PixelRGB8 0xFF 0xFF 0x00) = Right (Normal, Yellow)
pixeltocolor (PixelRGB8 0x00 0xFF 0x00) = Right (Normal, Green)
pixeltocolor (PixelRGB8 0x00 0xFF 0xFF) = Right (Normal, Cyan)
pixeltocolor (PixelRGB8 0x00 0x00 0xFF) = Right (Normal, Blue)
pixeltocolor (PixelRGB8 0xFF 0x00 0xFF) = Right (Normal, Magenta)
pixeltocolor (PixelRGB8 0xC0 0x00 0x00) = Right (Dark, Red)
pixeltocolor (PixelRGB8 0xC0 0xC0 0x00) = Right (Dark, Yellow)
pixeltocolor (PixelRGB8 0x00 0xC0 0x00) = Right (Dark, Green)
pixeltocolor (PixelRGB8 0x00 0xC0 0xC0) = Right (Dark, Cyan)
pixeltocolor (PixelRGB8 0x00 0x00 0xC0) = Right (Dark, Blue)
pixeltocolor (PixelRGB8 0xC0 0x00 0xC0) = Right (Dark, Magenta)
pixeltocolor (PixelRGB8 0xFF 0xFF 0xFF) = Right (Normal, White)
pixeltocolor (PixelRGB8 0x00 0x00 0x00) = Right (Normal, Black)
pixeltocolor x                          = Left $ "Error pixeltocolor: unknown color " ++ show x

decodeInstruction' :: Int -> Int -> Int -> PInstruction
decodeInstruction' 1 0 s = PPush s
decodeInstruction' 2 0 _ = PPop
decodeInstruction' 0 1 _ = PAdd
decodeInstruction' 1 1 _ = PSubstract
decodeInstruction' 2 1 _ = PMultiply
decodeInstruction' 0 2 _ = PDivide
decodeInstruction' 1 2 _ = PMod
decodeInstruction' 2 2 _ = PNot
decodeInstruction' 0 3 _ = PGreater
decodeInstruction' 1 3 _ = PPointer
decodeInstruction' 2 3 _ = PSwitch
decodeInstruction' 0 4 _ = PDuplicate
decodeInstruction' 1 4 _ = PRoll
decodeInstruction' 2 4 _ = PInNum
decodeInstruction' 0 5 _ = PInChar
decodeInstruction' 1 5 _ = POutNum
decodeInstruction' 2 5 _ = POutChar
decodeInstruction' l c s = error $ "Decodeinstruction error. Lightness diff=" ++ show l ++ ", Color diff=" ++ show c ++ ", val=" ++ show s

decodeInstruction :: (Lightness, Color, Int) -> (Lightness, Color) -> PInstruction
decodeInstruction (l1,c1, val) (l2,c2) =
    let lchange = ((fromEnum l2) - (fromEnum l1)) `mod` 3
        cchange = ((fromEnum c2) - (fromEnum c1)) `mod` 6
    in decodeInstruction' lchange cchange val


step :: (Int, Int) -> DP -> (Int, Int)
step (x,y) N = (x,y-1)
step (x,y) E = (x+1,y)
step (x,y) S = (x,y+1)
step (x,y) W = (x-1,y)

rot :: CC -> DP -> DP
rot CCLeft = toEnum . (`rem` 4) . (+4) . pred . fromEnum
rot CCRight = toEnum . (`rem` 4) . (+4) . succ . fromEnum

toggleCC :: CC -> CC
toggleCC CCLeft = CCRight
toggleCC CCRight = CCLeft

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours p = map (step p) [N,E,S,W]

getCodel :: Image (Lightness, Color) -> (Int, Int) -> ([(Int, Int)], Codel (Int, Int))
getCodel im startp = (pointsincodel, Codel lness clr size codelmap)
    where
        col@(lness, clr) = im A.! startp
        dfs :: (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
        dfs p s =
            let n   = filter (A.inRange (A.bounds im)) $ neighbours p
                n'  = filter (`S.notMember` s) n
                n'' = filter ((==col) . (im A.!)) n'
                s'  = S.insert p s
            in foldr dfs s' n''

        pointsincodel = S.toList $ dfs startp S.empty
        size = length pointsincodel
        -- TODO: Size is nu letterlijk aantal pixels. idealiter gaat dat automatisch
        -- Ofwel in getCodels (of ergens anders) de `pixel per codel' berkeenen en hier
        -- als paramaeter doorgooien en aanpassen. Anders kan ook ergens anders berekend 
        -- worden, en dan achteraf als we de graph hebben alle nodes aanpassen dat de 
        -- waardes geschaald moeten worden (of pas bij instructies maken of zo)
        -- TODO: Sowieso zou hier een flag voor moeten zijn om een geforceerde ding 
        -- pixels per codel te kunnen doen, maar moet ook gecheckt worden.

        getnext' :: [(Int, Int)] -> DP -> CC -> (Int, Int)
        getnext' points dp cc =
            let xy = if dp == N || dp == S then snd else fst
                sortt = if dp == S || dp == E then sortOn (Down . xy) else sortOn xy
                edge = head $ groupBy ((==) `on` xy) $ sortt points
            in case edge of
                [x] -> x
                xs  -> getnext' xs (rot cc dp) undefined
                
        getnextWhite :: (Int, Int) -> DP -> CC -> Maybe ((Int, Int), DP, CC)
        getnextWhite initP initDP initCC = whiteStep (initP, initDP, initCC) S.empty
            where
                whiteStep :: ((Int, Int), DP, CC) -> S.Set ((Int, Int), DP, CC) -> Maybe ((Int, Int), DP, CC)
                whiteStep x algeweest | S.member x algeweest = Nothing
                whiteStep x@(p, dp, cc) algeweest = 
                    let algeweest' = S.insert x algeweest
                        nextpunt = step p dp
                        inbound = A.inRange (A.bounds im) nextpunt
                        (_, nextpuntColor) = im A.! nextpunt
                    in if (not inbound) || (nextpuntColor == Black)
                        then whiteStep (p, rot CCRight dp, toggleCC cc) algeweest'
                        else if nextpuntColor == White
                                then whiteStep (nextpunt, dp, cc) algeweest'
                                else Just (nextpunt, dp, cc)                

        -- TODO: ook de oude methode van door witte blokken heengaan als optie doen 
        --       dus niet draaien in het witte blok, maar vanuit de andere cc opnieuw 
        --       vanuit het gekleurde blok gaan sliden
        -- TODO: Op deze manier is er een kans dat door een wit blok hij redirect naar zichzelf
        --       (wel met andere dp,cc waarschijnlijk, maar alsnog) zie bijvoorbeeld, Piet_piet
        getnext :: DP -> CC -> IndirType (Int, Int)
        getnext dp cc = 
            let startpunt = getnext' pointsincodel dp cc
                nextpunt = step startpunt dp
                (_, nextpuntColor) = im A.! nextpunt
            in if not (A.inRange (A.bounds im) nextpunt) then None else case nextpuntColor of
                Black   -> None
                White   -> case getnextWhite nextpunt dp cc of 
                                Nothing -> None
                                Just (punt, dp', cc') -> ThroughWhite punt dp' cc'
                _       -> Directly nextpunt

        codelmap = M.fromList [ ( (dp,cc), getnext dp cc) | dp <- [N,E,S,W], cc <- [CCLeft, CCRight] ]


getImage :: String -> IO (Either String (Image (Lightness, Color)))
getImage file = do
    ei <- readImage file
    case ei of
        Left err -> pure (Left err)
        Right im -> do
            let rgbim = convertRGB8 im
                w = imageWidth rgbim
                h = imageHeight rgbim
                f (x,y) = pixeltocolor $ pixelAt rgbim x y
                arr = A.genArray ((0,0),(w-1,h-1)) f
                arr' = sequenceA arr
            pure arr'


codelsFromImage :: String -> IO (Either String (ImageM (Codel (Int, Int)), S.Set (Codel (Int, Int))))
codelsFromImage = (pure . (getCodels <$>)) <=< getImage 
    


getCodels :: Image (Lightness, Color) -> (ImageM (Codel (Int, Int)), S.Set (Codel (Int, Int)))
getCodels im = foldr f (M.empty, S.empty) startQueue
    where
        startQueue = A.indices im

        f :: (Int, Int) -> (ImageM (Codel (Int, Int)), S.Set (Codel (Int, Int))) -> (ImageM (Codel (Int, Int)), S.Set (Codel (Int, Int)))
        f p (m,s) = 
            let (points, codel) = getCodel im p
            in if p `M.member` m 
                then (m,s) 
                else (foldr (\q -> M.insert q codel) m points, S.insert codel s)


codelMapToGraph :: (ImageM (Codel (Int, Int)), S.Set (Codel (Int, Int))) -> (G.Graph (Lightness, Color, Int) (DP,CC, IndirType ()), G.Vertex)
codelMapToGraph (im, s) =
    let vertices = map (\codel@(Codel l c size _) -> (codel,(l,c,size))) $ S.toList s
        makearcs codel@(Codel _ _ _ m) = map (\((dp,cc), p) -> (codel, im M.! (fromIndir p), (dp, cc, (const ()) <$> p))) 
                                        $ filter ((/= None) . snd) $ M.assocs m
        arcs = concatMap makearcs $ S.toList s
        (graph, labeltovertex) = G.fromList vertices arcs
        startnode = labeltovertex $ im M.! (0,0)
    in (graph, startnode)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c


data TMPInstruction = ORGEdge PInstruction | ORGNodeNop | ORGNodeStop deriving (Show)

instGraphFromCodelGraph1 :: G.Graph (Lightness, Color, Int) (DP,CC, IndirType ()) -> G.Graph [TMPInstruction] (DP,CC)
instGraphFromCodelGraph1 codelgraph = 
    let -- map alle arcs met G.mapArcs dat ze een extra Maybe Vertex ding hebben. 
        -- (initieel allemalal op Nothing)

        edges = G.allArcs codelgraph

        isColor v = let (_,c,_) = G.getVal v codelgraph in c /= Black && c /= White
        edges' = filter (\(v1,_,v2) -> isColor v1 && isColor v2) edges

                -- OKE HOE WE HET GAAN AANPAKKEN:
                {-
                    Map iedere edge naar een extra node tussen de twee nodes, met daarin instructie
                    of nop als het via niks is
                    Hiervoor moeten we nog wel iets van functionaliteit in DirGraph maken, misschien gewoon zo simpel als mapEdgeToVertex of zo

                    Dan, map iedere node naar NOP of STOP afhankelijk van hoeveel uitgaande edges het heeft

                    Tot slot: moeten we iets van een systeem bedenken om stukjes graaf te kunnen transformeren:
                        bijvoorbeeld: een switch insructgie zal een node met switch zijn met een enkele arc naar een node met NOP, maar wel met meerdere uitgaande edges
                        die moeten samengevoegd worden
                    dit is sowieso ook nodig, als we bijvoorbeeld pinhole dingen willen doen denk ik
                
                -}
        edgemapper arc@(v1, (d,c,it), v2)  = 
            let v1val@(l1,c1,val) = G.getVal v1 codelgraph
                (l2,c2,_) = G.getVal v2 codelgraph
                instruction = decodeInstruction v1val (l2,c2)
                arc' = (v1, (d,c), v2)
            in if c1 == Black || c2 == Black || c1 == White || c2 == White
                then uncurry3 G.removeArc arc'
                else case it of
                    None -> \g -> (uncurry3 G.removeArc arc') (error "asdfasdf" g) -- als het goed is komt deze nooit voor, 
                                            -- omdat in codelMapToGraph er al op gefiltert wordt: filter ((/= None) . snd)
                    ThroughWhite () dp cc -> \g ->
                        let g1 = uncurry3 G.removeArc arc' g
                            (nieuwevertex, g2) = G.insertVertex [ORGEdge PNop, ORGEdge $ PSetDP dp, ORGEdge $ PSetCC cc] g1
                            g3 = G.insertArc v1 nieuwevertex (d,c) g2
                            g4 = G.insertArc nieuwevertex v2 (dp,cc) g3
                        in g4
                    Directly () -> \g ->
                        let g1 = uncurry3 G.removeArc arc' g
                            (nieuwevertex, g2) = G.insertVertex [ORGEdge instruction ] g1
                            g3 = G.insertArc v1 nieuwevertex (d,c) g2
                            g4 = G.insertArc nieuwevertex v2 (d,c) g3
                        in g4

        graph' = G.mapVertices (\v ->
            let arcs = G.getArcs v codelgraph
                isNone (_, (_,_, None)) = True
                isNone _ = False
            in if all isNone arcs then [ORGNodeStop] else [ORGNodeNop]) codelgraph
        graph'' = G.mapArcs (\v1 v2 (dp,cc,typ) -> (dp,cc) ) graph'

        graphfunc = foldr1 (.) $ map edgemapper edges

    in graphfunc graph''

verify :: Eq e => G.Graph v e -> [(G.Vertex, e, G.Vertex)]
verify g = (G.allArcs g) \\ (G.allInArcs g)
verify2 :: Eq e => G.Graph v e -> [(G.Vertex, e, G.Vertex)]
verify2 g = (G.allArcs g) \\ ((G.allArcs g) \\ (G.allInArcs g))

nextseq :: (DP, CC) -> [(DP, CC)]
nextseq (d,c) = take 8 $ (d,c) : (d, toggleCC c) : nextseq (rot CCRight d, toggleCC c)

instGraphFromCodelGraph2 :: G.Graph [TMPInstruction] (DP,CC) -> G.Vertex -> G.Graph [PInstruction] (DP,CC)
instGraphFromCodelGraph2 codelgraph startnode = undefined

instGraphFromCodelGraph :: G.Graph (Lightness, Color, Int) (DP,CC, IndirType ()) -> G.Vertex -> G.Graph [PInstruction] (DP,CC)
instGraphFromCodelGraph = undefined

{-
Plan voor morgen:
    Graph omzetten in instructies
    Interpreter
    Dan misschien al meest simpele code generation?
        - voor iedere node een label in assembly, 
          en iedere label krijgt code voor instructie en dan case distinction op 
          DP,CC naar welke node die jumpt (of DP CC aanpast, dat kan ook, of gewoon stopt)
    Dan kunnen optimalisaties volgen:
        - virtuele stack
        - uitsluiten van sommige arcs, proberen zo lineair mogelijk te maken


-}