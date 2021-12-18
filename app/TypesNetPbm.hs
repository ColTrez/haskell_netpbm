module TypesNetPbm where

import qualified Data.ByteString as B
import Data.Word


data MagicNumber = P1 | P2 | P3 | P4 | P5 | P6
    deriving Show

prettyPrintMagicNum :: MagicNumber -> String
prettyPrintMagicNum P1 = "P1 - black and white ASCII"
prettyPrintMagicNum P2 = "P2 - Grayscale ASCII"
prettyPrintMagicNum P3 = "P3 - RGB ASCII"
prettyPrintMagicNum P4 = "P4 - black and white RAW"
prettyPrintMagicNum P5 = "P5 - Grayscale RAW"
prettyPrintMagicNum P6 = "P6 - RGB RAW"



data Image = Image { filetype :: MagicNumber
                   , width :: Int
                   , height :: Int
                   , maxVal :: Int
                   , comments :: [String]
                   , image :: B.ByteString
                   }

formatImageHeader :: Image -> String
formatImageHeader image =
    let magicNum = show $ filetype image
        dimensions = (show $ width image) ++ " " ++ (show $ height image)
        maxValue = show $ maxVal image
        comm = concat $ comments image
    in concat $ map (++ "\n") [magicNum, comm, dimensions, maxValue] 

instance Show Image where
    show (Image mn w h mv cs _) = "Type: " ++ (prettyPrintMagicNum mn) ++ "\nWidth: " ++(show w) ++
        "\nHeight: " ++ (show h) ++ "\nMax Value: " ++ (show mv) ++
            "\nComments: " ++ (show $ length cs) ++ " comments\n" ++ printComments cs
        where
            printComments :: [String] -> String
            printComments [] = ""
            printComments (x:xs) = (x ++ "\n") ++ printComments xs

-- types for input verification
data FileType = PBM | PGM | PPM
    deriving Show

data FileMode = RAW | ASCII
    deriving Show

data RGBPixel = Pixel {red :: Word8, green :: Word8, blue :: Word8}
