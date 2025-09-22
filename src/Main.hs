module Main (main) where

import Codec.Picture hiding (Image)
import qualified Data.Array.IArray as A
import qualified Data.Map as M -- TODO Intmap of Array of Vector

import qualified Data.Queue as Q

type Image a = A.Array (Int, Int) a
type ImageM a = M.Map (Int, Int) a

data DP = N | E | S | W deriving (Show)
data CC = CCLeft | CCRight deriving (Show)
data Codel a = Codel {
    lightness :: Lightness,
    color :: Color,
    nextCodel :: M.Map (DP,CC) (Maybe a)
}




data Color = Red | Yellow | Green | Cyan | Blue | Magenta | White | Black deriving (Show)
data Lightness = Light | Normal | Dark deriving (Show)

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


getImage :: IO (Maybe (Image (Lightness, Color)))
getImage = do
    ei <- readImage "./tests/Piet_hello.png"
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


getCodels :: Image (Lightness, Color) -> ImageM (Codel (Int, Int))
getCodels im = undefined
    where
        startQueue = Q.fromList $ A.indices im



main :: IO ()
main = do
    getImage >>= print
