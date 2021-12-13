module Netpbm where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char (isSpace)

{-
    Types for identifying filetype
    PBM filetype: black and white
    P1 - pbm Ascii
    p4 - pbm Raw
    PGM filetype: Grayscale
    P2 - pgm Ascii
    P5 - pgm Raw
    PPM filetype: RGB
    P3 - ppm Ascii
    P6 - ppm Raw
-}
    
data MagicNumber = P1 | P2 | P3 | P4 | P5 | P6
    deriving (Show)

data Image = Image { filetype :: MagicNumber
                   , width :: Int
                   , height :: Int
                   , maxVal :: Int
                   , comments :: [String]
                   }

instance Show Image where
    show (Image mn w h mv cs) = "Type: " ++ (show mn) ++ "\nWidth: " ++(show w) ++
        "\n Height: " ++ (show h) ++ "\n Max Value: " ++ (show mv) ++ printComments cs
        where
            printComments :: [String] -> String
            printComments [] = ""
            printComments (x:xs) = (x ++ "\n") ++ printComments xs

magicNumFromBytes :: B.ByteString -> Either String MagicNumber
magicNumFromBytes bytes = let
                             prefix = C8.unpack bytes
                          in case prefix of
                               "P1" -> Right P1
                               "P2" -> Right P2
                               "P3" -> Right P3
                               "P4" -> Right P4
                               "P5" -> Right P5
                               "P6" -> Right P6
                               otherwise -> Left ("Couldn't parse magic number. Got: " ++ prefix)

parseMagicNumber :: B.ByteString -> Either String (MagicNumber, B.ByteString)
parseMagicNumber bytes = let
                             (maybeMn, rest) = B.splitAt 2 bytes
                             mn = magicNumFromBytes maybeMn 
                         in case mn of
                              Left err -> Left err
                              Right magicNumber -> Right (magicNumber, rest)
                             
