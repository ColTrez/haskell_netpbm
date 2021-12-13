module Main where

import Netpbm
import qualified Data.ByteString as B

main :: IO ()
main = do
    file <- B.readFile filePath
    let maybeMagicNum = parseMagicNumber file
    case maybeMagicNum of
      Left err -> print $ "Error: " ++ err
      Right (mn, file') -> print $ show $ fst $ parseComments (dropLeadingWhiteSpace file')
          --print $ "Magic number is " ++ (show mn)
          --let (comments, file'') = (parseComments file')
          --in (print $ "Comments are: " ++ (show comments)) 
                               

filePath :: FilePath
filePath = "./testImages/smileAscii.pbm"
