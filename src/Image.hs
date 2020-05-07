module Image where

import Codec.Picture.Types
import Codec.Picture.Bitmap
import Control.Monad
import Control.Type.Operator
import Data.Matrix
import Data.Function
import Data.Word
import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as VS

import Dungeon
import Misc

type Bitmap = Image PixelRGBA8

-- https://en.wikipedia.org/wiki/Alpha_compositing
-- short-circuiting
alphaComposite pxTop pxBot
    | aTop == maxBound = pxTop
    | aTop == minBound = pxBot
    | otherwise = mixWithAlpha (const (compColor `on` w8ToF))
                               (compAlpha `on` w8ToF)
                                pxTop pxBot
  where
    aTop  = pixelOpacity pxTop
    aBot  = pixelOpacity pxBot
    w8ToF w = realToFrac w / realToFrac (maxBound :: Word8)
    fToW8 f = truncate $ f * realToFrac (maxBound :: Word8)
    aTRat = w8ToF aTop
    aBRat = w8ToF aBot
    compColor vT vB = fToW8 $ vT*aTRat + vB*aBRat*(1-aTRat)
    compAlpha vT vB = fToW8 $ aTRat + aBRat*(1-aTRat)

drawGridLines :: Int -> Int -> Int -> Int -> PixelRGBA8 -> Bitmap -> Bitmap
drawGridLines tileX tileY offX offY px = pixelMapXY $ \x y ->
  if (x + offX) `mod` tileX == 0 || (y + offY) `mod` tileY == 0
    then alphaComposite px
    else id

-- Assumes all tiles have the same dimension
flattenGrid :: Maybe PixelRGBA8 -> Matrix $ Bitmap -> Bitmap
flattenGrid pxM g
    | nrows g == 0 = emptyImage
    | otherwise    = maybe id (drawGridLines tileX tileY 0 0) pxM
                   $ generateImage getPixel ((ncols g) * tileX) ((nrows g) * tileY)
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

toBitmap :: String -> String -> String -> Maybe PixelRGBA8 -> Dungeon
         -> IO $ Either String Bitmap
toBitmap wallFile roomFile hallFile pxM dun = do
  wallE <- readBitmap wallFile
  roomE <- readBitmap roomFile
  hallE <- readBitmap hallFile
  return $ do
    wall <- wallE
    room <- roomE
    hall <- hallE
    return $ flattenGrid pxM $ dunTile wall room hall <$> dun

genImage :: String -> Maybe PixelRGBA8 -> Dungeon -> IO $ Maybe String
genImage file pxM dun = toBitmap "wall.bmp" "room.bmp" "hall.bmp" pxM dun
                        >>= either (return . Just)
                            (fmap (const Nothing) . writeBitmap file)
