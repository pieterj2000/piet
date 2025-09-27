module Main (main) where

import Codec.Picture hiding (Image)
import qualified Data.Array.IArray as A
import qualified Data.Map as M -- TODO Intmap of Array of Vector
import qualified Data.Set as S
import qualified Data.Queue as Q
import Data.Function (on)
import Data.Ord (comparing, Down (Down))
import Data.List (sortOn, group, groupBy)


import qualified Debug.Trace as Debug

type Image a = A.Array (Int, Int) a
type ImageM a = M.Map (Int, Int) a

data DP = N | E | S | W deriving (Eq, Show, Enum, Ord)
data CC = CCLeft | CCRight deriving (Eq, Show, Ord)
data Codel a = Codel {
    lightness :: Lightness,
    color :: Color,
    nextCodel :: M.Map (DP,CC) (Maybe a)
} deriving (Show)




data Color = Red | Yellow | Green | Cyan | Blue | Magenta | White | Black deriving (Eq, Show)
data Lightness = Light | Normal | Dark deriving (Eq, Show)

pixeltocolor :: PixelRGB8 -> Maybe (Lightness, Color)
pixeltocolor (PixelRGB8 0xFF 0xC0 0xC0) = Just (Light, Red)
pixeltocolor (PixelRGB8 0xFF 0xFF 0xC0) = Just (Light, Yellow)
pixeltocolor (PixelRGB8 0xC0 0xFF 0xC0) = Just (Light, Green)
pixeltocolor (PixelRGB8 0xC0 0xFF 0xFF) = Just (Light, Cyan)
pixeltocolor (PixelRGB8 0xC0 0xC0 0xFF) = Just (Light, Blue)
pixeltocolor (PixelRGB8 0xFF 0xC0 0xFF) = Just (Light, Magenta)
pixeltocolor (PixelRGB8 0xFF 0x00 0x00) = Just (Normal, Red)
pixeltocolor (PixelRGB8 0xFF 0xFF 0x00) = Just (Normal, Yellow)
pixeltocolor (PixelRGB8 0x00 0xFF 0x00) = Just (Normal, Green)
pixeltocolor (PixelRGB8 0x00 0xFF 0xFF) = Just (Normal, Cyan)
pixeltocolor (PixelRGB8 0x00 0x00 0xFF) = Just (Normal, Blue)
pixeltocolor (PixelRGB8 0xFF 0x00 0xFF) = Just (Normal, Magenta)
pixeltocolor (PixelRGB8 0xC0 0x00 0x00) = Just (Dark, Red)
pixeltocolor (PixelRGB8 0xC0 0xC0 0x00) = Just (Dark, Yellow)
pixeltocolor (PixelRGB8 0x00 0xC0 0x00) = Just (Dark, Green)
pixeltocolor (PixelRGB8 0x00 0xC0 0xC0) = Just (Dark, Cyan)
pixeltocolor (PixelRGB8 0x00 0x00 0xC0) = Just (Dark, Blue)
pixeltocolor (PixelRGB8 0xC0 0x00 0xC0) = Just (Dark, Magenta)
pixeltocolor (PixelRGB8 0xFF 0xFF 0xFF) = Just (Normal, White)
pixeltocolor (PixelRGB8 0x00 0x00 0x00) = Just (Normal, Black)
pixeltocolor _                          = Nothing


getImage :: String -> IO (Maybe (Image (Lightness, Color)))
getImage file = do
    ei <- readImage file
    case ei of
        Left err -> putStrLn err >> pure Nothing
        Right im -> do
            let rgbim = convertRGB8 im
                w = imageWidth rgbim
                h = imageHeight rgbim
                f (x,y) = pixeltocolor $ pixelAt rgbim x y
                arr = A.genArray ((0,0),(w-1,h-1)) f
                arr' = sequenceA arr
            pure arr'

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





getCodels :: Image (Lightness, Color) -> ImageM (Codel (Int, Int))
getCodels im = foldr f M.empty startQueue
    where
        startQueue = A.indices im

        f :: (Int, Int) -> ImageM (Codel (Int, Int)) -> ImageM (Codel (Int, Int))
        f p m = 
            let (points, codel) = getCodel im p 
            in foldr (\q -> M.insert q codel) m points






main :: IO ()
main = do
    ima <- getImage "./tests/Piet_hello_small.png" -- "./tests/Piet_hello.png"
    print ima
    case ima of
        Nothing -> return ()
        Just im -> do
            -- let (list, cdl) = getCodel im (0,0)
            -- print list
            -- print (lightness cdl)
            -- print $ nextCodel cdl
            let m = getCodels im
            print m