module Main where

import Netpbm
import qualified Data.ByteString as B

main :: IO ()
main = do
    file <- B.readFile filePath
    let maybeMagicNum = parseMagicNumber file
    case maybeMagicNum of
      Left err -> print $ "Error: " ++ err
      Right (mn, moreBytes) -> print $ "Magic number is " ++ (show mn)

filePath :: FilePath
filePath = "./testImages/dummyFile"
