module ConvertNetPbm where

import TypesNetPbm
import ParserNetPbm
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Word
import System.IO


convertImage :: Image -> FileType -> FileMode -> IO ()
convertImage img ft fm = let newImage = ppmToPgm $ image img
                             targetType = findTargetMagicNum ft fm
                             newFile = (convertHeader targetType img) newImage
                         in writeImage newFile

findTargetMagicNum :: FileType -> FileMode -> MagicNumber
findTargetMagicNum ft fm =
    case ft of
      PBM -> case fm of
               ASCII -> P1
               RAW -> P4
      PGM -> case fm of
               ASCII -> P2
               RAW -> P5
      PPM -> case fm of
               ASCII -> P3
               RAW -> P6

getTargetMaxVal :: MagicNumber -> Int
getTargetMaxVal mn =
    case mn of
      P1 -> 1
      P4 -> 1
      otherwise -> 255
      

convertHeader :: MagicNumber -> Image -> (B.ByteString -> Image)
convertHeader targetType oldImage =
    Image targetType (width oldImage) (height oldImage) (getTargetMaxVal targetType) (comments oldImage)

writeImage :: Image -> IO ()
writeImage img = let newFile = "converted.pgm"
                     header = formatImageHeader img
                     newImage = image img
                 in do
                     writeFile newFile header
                     B.appendFile newFile newImage


asciiToRaw :: B.ByteString -> B.ByteString
asciiToRaw = B.pack . (map read) . concat . (map words) . lines . C8.unpack

rawToAscii :: B.ByteString -> B.ByteString
rawToAscii = C8.pack . concat . (map (++ "\n")) . (map show) . B.unpack 

type GrayPixel = Word8

rgbToGrayPixel :: RGBPixel -> GrayPixel
rgbToGrayPixel rgb = 
    let r = fromIntegral $ red rgb
        g = fromIntegral $ green rgb
        b = fromIntegral $ blue rgb
--    in 0
    in ((r `div` 3) + (g `div` 3) + (b `div` 3))

--assumes raw
ppmToPgm :: B.ByteString -> B.ByteString
ppmToPgm ppmImage =
    let rgbPixels = parseRGBPixels ppmImage
        greyPixels = map rgbToGrayPixel rgbPixels
    in B.pack greyPixels
