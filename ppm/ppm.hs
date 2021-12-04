import System.IO
import qualified Data.ByteString as B
import Data.Word
import System.Random

main = do
    let dimensions = (50,50)
        columns = fst dimensions
        rows = snd dimensions
        magicNumber = "P6"
        header = concat [magicNumber, "\n", show columns, " "
                        , show rows, "\n", show channelMax, "\n"]
        file = "img.ppm"
        pixelCount = rows * columns
    writeFile file header
    gen <- getStdGen
    let image = B.pack $ take (3 * (rows * columns)) $ clownVomit gen channelMax
    B.appendFile file image

channelMax = 255

clownVomit :: StdGen -> Word8 -> [Word8]
clownVomit gen chnlMax = randomRs (0,chnlMax) gen
