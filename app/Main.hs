module Main where

import ParserNetPbm
import TypesNetPbm

import qualified Data.ByteString as B

main :: IO ()
main = do
    file <- B.readFile filePath
    let image = parseImage file
    case image of
      Left err -> print err
      Right img -> print img
                               

filePath :: FilePath
filePath = "./testImages/smileAscii.pbm"
