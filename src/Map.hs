module Map where

import System.Random
import Control.Type.Operator
import Data.Maybe

import Matrix

data MapElem = Wall | Room | Hall
  deriving Eq

mapElem a b c = go
  where
    go Wall = a
    go Room = b
    go Hall = c

-- *inclusive* range check
x `inRange` (min, max) = x >= min && x <= max

genMap :: Integer -> Integer -> Integer -> IO $ Matrix MapElem
genMap gas sizeX sizeY = go gas $ matrix sizeY sizeX $ curry $ const Wall
  where
    sizeMin = 3
    sizeMax = 10

    go :: Integer -> Matrix MapElem -> IO $ Matrix MapElem
    go 0 map = return map
    go n map = do
      room <- genRoom
      let overlay = zipMatrix room map
      if any collision overlay
        then go (n-1) map
        else go gas $ flatten overlay
      where 
        collision (mx, y) = isJust mx && y == Room
        flatten = fmap $ \(mx, y) -> maybe y id mx

    genRoom :: IO $ Matrix $ Maybe MapElem
    genRoom = do
      roomSizeX <- randomRIO (sizeMin, sizeMax)
      roomSizeY <- randomRIO (sizeMin, sizeMax)
      roomX <- randomRIO (0, sizeX - roomSizeX)
      roomY <- randomRIO (0, sizeY - roomSizeY)
      return $ matrix sizeY sizeX $ \x y ->
        if x `inRange` (roomX, roomX + roomSizeX -1) &&
           y `inRange` (roomY, roomY + roomSizeY -1)
        then Just Room else
        if x `inRange` (roomX -1, roomX + roomSizeX) &&
           y `inRange` (roomY -1, roomY + roomSizeY)
        then Just Wall else 
        Nothing
