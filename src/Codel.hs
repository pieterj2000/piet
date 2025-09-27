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
data Codel a = Codel {
    lightness :: Lightness,
    color :: Color,
    nextCodel :: M.Map (DP,CC) (Maybe a)
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

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours p = map (step p) [N,E,S,W]

getCodel :: Image (Lightness, Color) -> (Int, Int) -> ([(Int, Int)], Codel (Int, Int))
getCodel im startp = (pointsincodel, Codel lness clr codelmap)
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

        getnext' :: [(Int, Int)] -> DP -> CC -> (Int, Int)
        getnext' points dp cc =
            let xy = if dp == N || dp == S then snd else fst
                sortt = if dp == S || dp == E then sortOn (Down . xy) else sortOn xy
                edge = head $ groupBy ((==) `on` xy) $ sortt points
            in case edge of
                [x] -> x
                xs  -> getnext' xs (rot cc dp) undefined
                
        getnext :: DP -> CC -> Maybe (Int, Int)
        getnext dp cc = 
            let startpunt = getnext' pointsincodel dp cc
                nextreeks = tail $ iterate (\p -> step p dp) startpunt
                nextreeks' = takeWhile (A.inRange (A.bounds im)) nextreeks
                nextreeks'' = dropWhile ((==White) . snd . (im A.!)) nextreeks'
            in case nextreeks'' of
                [] -> Nothing
                (p:_) -> if snd (im A.! p) == Black
                    then Nothing
                    else Just p

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


codelMapToGraph :: (ImageM (Codel (Int, Int)), S.Set (Codel (Int, Int))) -> (G.Graph (Lightness, Color) (DP,CC), G.Vertex)
codelMapToGraph (im, s) =
    let vertices = map (\codel@(Codel l c _) -> (codel,(l,c))) $ S.toList s
        makearcs codel@(Codel _ _ m) = map (\(dir, Just p) -> (codel, im M.! p, dir)) $ filter (isJust . snd) $ M.assocs m
        arcs = concatMap makearcs $ S.toList s
        (graph, labeltovertex) = G.fromList vertices arcs
        startnode = labeltovertex $ im M.! (0,0)
    in (graph, startnode)