module ParserNetPbm where

import TypesNetPbm

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char (isSpace)

dropLeadingWhiteSpace :: B.ByteString -> B.ByteString
dropLeadingWhiteSpace bytes
  | isSpace (C8.head bytes) = dropLeadingWhiteSpace (B.tail bytes)
  | otherwise               = bytes

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

parseComments :: B.ByteString -> ([String], B.ByteString)
parseComments bytes = let 
                        firstByte = C8.head bytes
                      in case firstByte of
                           '#' -> let (comment, rest) = C8.span (\c -> c /= '\n') bytes
                                      recurs = parseComments (dropLeadingWhiteSpace rest)
                                  in (C8.unpack comment : fst recurs, snd recurs)
                           otherwise -> ([], bytes)

parseInt :: B.ByteString -> Either String (Int, B.ByteString)
parseInt bytes = case (C8.readInt bytes) of
                   Nothing -> Left "Couldn't parse int"
                   Just result -> Right result

parseDimensions :: B.ByteString -> Either String ((Int,Int), B.ByteString)
parseDimensions bytes = do
    (width, afterWidth) <- parseInt bytes
    (height, afterHeight) <- parseInt (dropLeadingWhiteSpace afterWidth)
    return ((width, height), afterHeight)
                                               
parseImage :: B.ByteString -> Either String Image
parseImage bytes = do
    magicNum <- parseMagicNumber bytes
    parseFromMagicNum magicNum

parseFromMagicNum :: (MagicNumber, B.ByteString) -> Either String Image
parseFromMagicNum (P1, bytes) = parsePBMImage (P1, bytes)
parseFromMagicNum (P4, bytes) = parsePBMImage (P4, bytes)
parseFromMagicNum (mn, bytes) = do
    let afterMn = dropLeadingWhiteSpace bytes
    let (comments, bytes') = parseComments afterMn
    let afterComments = dropLeadingWhiteSpace bytes'
    ((width, height), bytes'') <- parseDimensions afterComments
    let afterDimensions = dropLeadingWhiteSpace bytes''
    (maxVal, rest) <- parseInt afterDimensions
    let image = dropLeadingWhiteSpace rest
    return $ Image mn width height maxVal comments image

--pbms dont have a max value so they get special treatment
parsePBMImage :: (MagicNumber, B.ByteString) -> Either String Image
parsePBMImage (mn, bytes) = do
    let afterMn = dropLeadingWhiteSpace bytes
    let (comments, bytes') = parseComments afterMn
    let afterComments = dropLeadingWhiteSpace bytes'
    ((width, height), rest) <- parseDimensions afterComments
    let image = dropLeadingWhiteSpace rest
    return $ Image mn width height 1 comments image


-- Functions for parsing input arguments
parseFileTypeArg :: String -> Either String FileType
parseFileTypeArg arg
    | arg == "pbm" = Right PBM
    | arg == "pgm" = Right PGM
    | arg == "ppm" = Right PPM
    | otherwise = Left $ "Error: Filetype \"" ++ arg ++ "\" not recognized, use pbm, pgm, or ppm"

parseFileModeArg :: String -> Either String FileMode
parseFileModeArg arg
    | arg == "raw" = Right RAW
    | arg == "RAW" = Right RAW
    | arg == "ascii" = Right ASCII
    | arg == "ASCII" = Right ASCII
    | otherwise = Left $ "Error: File mode \"" ++ arg ++ "\" not recognized, use raw or ascii"

parseRGBPixel :: B.ByteString -> Maybe (RGBPixel, B.ByteString)
parseRGBPixel bytes
    | B.length bytes < 3 = Nothing
    | otherwise = let [r, g, b] = B.unpack $ B.take 3 bytes
                  in Just (Pixel r g b, B.drop 3 bytes)

parseRGBPixels :: B.ByteString -> [RGBPixel]
parseRGBPixels bytes =
    case parseRGBPixel bytes of
      Nothing -> []
      Just (pxl, bytes') -> pxl : (parseRGBPixels bytes')
