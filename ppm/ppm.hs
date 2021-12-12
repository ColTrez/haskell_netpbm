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
    let image = B.concat $ map pixelBytes $ take pixelCount $ purpleStatic gen
    --let image = B.pack $ take (3 * (rows * columns)) $ clownVomit gen channelMax
    B.appendFile file image

data Pixel = Pixel { red :: Word8, green :: Word8, blue :: Word8 } deriving Show

pixelBytes :: Pixel -> B.ByteString
pixelBytes p = let pixels = [red p, green p, blue p]
               in  B.pack pixels

channelMax = 255

randomBytes :: StdGen -> [Word8]
randomBytes gen = randomRs (10,channelMax) gen

purpleStatic :: StdGen -> [Pixel]
purpleStatic gen = map (\w -> Pixel w 0 w) $ randomBytes gen

clownVomit :: StdGen -> Word8 -> [Word8]
clownVomit gen chnlMax = randomRs (0,chnlMax) gen


