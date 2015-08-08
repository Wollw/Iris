import Codec.Picture
import Codec.Picture.Types (promoteImage)
import qualified Data.Vector.Storable as V
import System.IO
import PCD.Data
import PCD.Header
import qualified Data.Text as T
import Control.Lens
import System.Environment (getArgs)
import Data.Bits
import Unsafe.Coerce

main = do
    args <- getArgs
    ri <- readImage $ args !! 0
    case ri of
        Left err -> hPutStrLn stderr err
        Right di -> makeSavePcd di $ args !! 1

makeSavePcd (ImageRGB8 i) f = makeSavePcd' i f
makeSavePcd _ _ = hPutStrLn stderr "unsupported file format"
makeSavePcd' i outFile = saveBinaryPcd
        outFile
        (makePcdHeader i)
        $ imageData (promoteImage i :: Image PixelRGBF)

makePcdHeader (Image w h _) = defaultHeader
    & version  .~ defaultVersion
    & fields   .~ map T.pack ["x","y","z"]
    & sizes    .~ [4,4,4]
    & dimTypes .~ [F,F,F]
    & counts   .~ [1,1,1]
    & width    .~ fromIntegral w
    & height   .~ fromIntegral h
    & points   .~ fromIntegral (w * h)
    & format   .~ Binary
