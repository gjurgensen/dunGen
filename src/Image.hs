module Image where

import Codec.Picture.Types
import Codec.Picture.Bitmap
import Control.Monad
import Control.Type.Operator
import Data.Matrix
import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as VS

import Dungeon
import Misc

type Bitmap = Image PixelRGBA8

-- Assumes all tiles have the same dimension
flattenGrid :: Pixel px => Matrix $ Image px -> Image px
flattenGrid g
    | nrows g == 0 = emptyImage
    | otherwise    = generateImage getPixel ((ncols g) * tileX) ((nrows g) * tileY)
  where
    emptyImage = Image 0 0 VS.empty
    firstTile = adjGetElem g 0 0
    tileX = imageWidth  $ firstTile
    tileY = imageHeight $ firstTile
    getPixel x y = pixelAt (adjGetElem g gx gy) px py
      where
        (gx,px) = x `divMod` tileX
        (gy,py) = y `divMod` tileY

readBitmap :: FilePath -> IO $ Either String Bitmap
readBitmap file = toStatic <$> decodeBitmap <$> BS.readFile file
  where
    toStatic :: Either String DynamicImage -> Either String Bitmap
    toStatic (Right (ImageRGBA8 i)) = Right i
    toStatic (Right (ImageRGB8  i)) = Right $ promoteImage i
    toStatic (Right (ImageY8    i)) = Right $ promoteImage i
    toStatic (Right _) = Left "Incompatible pixel type"
    toStatic (Left  l) = Left l

toBitmap :: String -> String -> String -> Dungeon -> IO $ Either String Bitmap
toBitmap wallFile roomFile hallFile dun = do
  wallE <- readBitmap wallFile
  roomE <- readBitmap roomFile
  hallE <- readBitmap hallFile
  return $ do
    wall <- wallE
    room <- roomE
    hall <- hallE
    return $ flattenGrid $ dunTile wall room hall <$> dun

genImage :: String -> Dungeon -> IO $ Maybe String
genImage file dun = toBitmap "wall.bmp" "room.bmp" "hall.bmp" dun
                    >>= either (return . Just)
                        (fmap (const Nothing) . writeBitmap file)
