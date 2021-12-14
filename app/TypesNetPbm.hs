module TypesNetPbm where

import qualified Data.ByteString as B


data MagicNumber = P1 | P2 | P3 | P4 | P5 | P6

instance Show MagicNumber where
    show P1 = "P1 - black and white ASCII"
    show P2 = "P2 - Grayscale ASCII"
    show P3 = "P3 - RGB ASCII"
    show P4 = "P4 - black and white RAW"
    show P5 = "P5 - Grayscale RAW"
    show P6 = "P6 - RGB RAW"



data Image = Image { filetype :: MagicNumber
                   , width :: Int
                   , height :: Int
                   , maxVal :: Int
                   , comments :: [String]
                   , image :: B.ByteString
                   }

instance Show Image where
    show (Image mn w h mv cs _) = "Type: " ++ (show mn) ++ "\nWidth: " ++(show w) ++
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


