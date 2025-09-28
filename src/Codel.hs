{-# LANGUAGE InstanceSigs #-}
module Codel (
    Color,
    Lightness,
    Image,
    codelsFromImage,
    codelMapToGraph
) where

    
import qualified Data.Array.IArray as A
import qualified Data.Map as M -- TODO Intmap of Array of Vector
import qualified Data.Set as S
import Codec.Picture hiding (Image)
import Data.Function (on)
import Data.List (sortOn, groupBy)
import Data.Ord (Down (Down))
import Control.Monad ((<=<))
import qualified Data.DirGraph as G
import Data.Maybe (isJust)

    
type Image a = A.Array (Int, Int) a
type ImageM a = M.Map (Int, Int) a

data DP = N | E | S | W deriving (Eq, Show, Enum, Ord)
data CC = CCLeft | CCRight deriving (Eq, Show, Ord)
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

data Color = Red | Yellow | Green | Cyan | Blue | Magenta | White | Black deriving (Eq, Show, Ord)
data Lightness = Light | Normal | Dark deriving (Eq, Show, Ord)

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