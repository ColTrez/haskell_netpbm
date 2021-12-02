import System.IO
import qualified Data.ByteString as B
import Data.Word

main = do
    fileContents <- B.readFile "smile.pbm"
    let (magicNumber, contents) = B.span notNewline fileContents
    let withoutComments = ditchComments contents
    let (dimensionsRaw, rest) = B.span notNewline withoutComments
    let image = clearNewLine rest
    let dimensionsStrings = words . filter (/= '\"') $ show dimensionsRaw
    let (rows, columns) = 
            (read (dimensionsStrings !! 0) :: Int, read (dimensionsStrings !! 1) :: Int)
    
    let speckled = B.map (\w -> if w == 0 then w + 37 else w) image
    let nl = 10 :: Word8 --ascii code for newline
    let newImage = B.concat [B.snoc magicNumber nl, B.snoc dimensionsRaw nl, speckled]
    B.writeFile "./edit.pbm" newImage
   
notNewline :: Word8 -> Bool
notNewline w = (w /= nl)
            where nl = 10 --ascii code for newline

clearNewLine :: B.ByteString -> B.ByteString
clearNewLine = snd . B.break notNewline

ditchComments :: B.ByteString -> B.ByteString
ditchComments b = let clearNewLine = snd . B.break notNewline
                      b' = clearNewLine b
                      (maybeComment, contents) = B.span notNewline b'
                      commentChar = 35 --ascii code for '#' which starts a comment
                      isComment = B.head maybeComment == commentChar
                   in if isComment then ditchComments contents else b'
