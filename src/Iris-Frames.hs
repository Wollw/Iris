{-# LANGUAGE BangPatterns #-}
import Codec.Picture
import Codec.Picture.Types (promoteImage)
import qualified Data.Vector.Storable as V
import System.IO
import System.Environment (getArgs)
import Data.Bits
import Unsafe.Coerce
import Data.List.Split
import Text.Printf

myFilter (r:g:b:[]) = r `div` 2 + g `div` (if b /= 0 then b else 1)

main = do
    args <- getArgs
    ri <- readImage $ args !! 0
    case ri of
        Left err -> hPutStrLn stderr err
        Right di -> makeFrames myFilter di $ args !! 1


makeFrames f (ImageRGB8 i@(Image w h d)) outDir = do
    let ld = (map fromIntegral $ V.toList d) 
    let vd = zipWith (\a b -> (a,b))
            (map f $ chunksOf 3 ld)
            (chunksOf 3 ld)
    let values = [ map (\(a,b) -> if a == i then b++[255] else [0,0,0,0]) vd | i <- [0..(maxKey vd)]]
    let frames = map (Image w h . V.fromList . map fromIntegral) $ map concat values
    putStrLn $ show . length $ vd
    saveFrames (0::Integer) frames
  where
    saveFrames n (f:fs) = do putStrLn $ "Frames Left " ++ (show $ length fs )
                             savePngImage outFile $ ImageRGBA8 f
                             saveFrames (n+1) fs
        where outFile :: String
              outFile = fileNameOf outDir n
    saveFrames _ []   = return ()
    maxKey = foldr (\(a,_) b -> max a b) 0
makeFrames _ _ _ = hPutStrLn stderr "unsupported file format"

fileNameOf :: (Integral a, PrintfArg a) => String -> a -> String
fileNameOf dir n = printf "%s/%09d.png" dir n
