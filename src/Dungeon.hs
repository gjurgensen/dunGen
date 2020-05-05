module Dungeon where

import System.Random
import Control.Type.Operator
import Data.Matrix
import Data.Maybe

data DunTile = Wall | Room | Hall
  deriving Eq

dunTile a b c = go
  where
    go Wall = a
    go Room = b
    go Hall = c

type Dungeon = Matrix DunTile

-- *inclusive* range check
x `inRange` (min, max) = x >= min && x <= max
(x, y) `inRect` ((minX, minY), (maxX, maxY)) = x `inRange` (minX, maxX)
                                            && y `inRange` (minY, maxY)

mx `orElse` y = maybe y id mx

cmdlineRender :: Dungeon -> IO ()
cmdlineRender = putStrLn . unlines
              . fmap (unwords . (fmap $ dunTile " " "#" "x")) . toLists

dunGen :: Int -> Int -> Int -> IO Dungeon
dunGen gas sizeX sizeY = go gas $ matrix sizeY sizeX $ const Wall
  where
    sizeMin = 3
    sizeMax = 10

    go :: Int -> Dungeon -> IO Dungeon
    go 0 dun = return dun
    go n dun = do
      room <- genRoom
      if or $ elementwise collision room dun
        then go (n-1) dun
        else go gas $ combine room dun
      where
        collision mx y = isJust mx && y == Room
        combine = elementwise orElse

    genRoom :: IO $ Matrix $ Maybe DunTile
    genRoom = do
      roomSizeX <- randomRIO (sizeMin, sizeMax)
      roomSizeY <- randomRIO (sizeMin, sizeMax)
      roomX <- randomRIO (0, sizeX - roomSizeX)
      roomY <- randomRIO (0, sizeY - roomSizeY)
      return $ matrix sizeY sizeX $ \p ->
        if p `inRect`
          ((roomX, roomY), (roomX + roomSizeX -1, roomY + roomSizeY -1))
        then Just Room else
        if p `inRect`
          ((roomX -1, roomY -1), (roomX + roomSizeX, roomY + roomSizeY))
        then Just Wall else
        Nothing
