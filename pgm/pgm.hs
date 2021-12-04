import System.IO
import qualified Data.ByteString as B
import Data.Word
import System.Random

main = do
    let dimensions = (100,100)
        columns = fst dimensions
        rows = snd dimensions
        bitsPerPixel = 8
        magicNumber = "P5"
        header = concat [magicNumber, "\n", show columns, " "
                        , show rows, "\n", show bitsPerPixel, "\n"]
        file = "img.pgm"
    writeFile file header
    gen <- getStdGen
    let image = B.pack $ take (rows * columns) $ generateStatic gen bitsPerPixel
    --let image = B.concat $ take rows $ repeat $ makeRow 0 columns
    B.appendFile file image

makeRow :: Word8 -> Int -> B.ByteString
makeRow w n = B.pack $ take n $ repeat w

generateStatic :: StdGen -> Word8 -> [Word8]
generateStatic gen max = randomRs (0,max) gen
