module Dungeon where

import System.Random
import Control.Type.Operator
import Data.Matrix
import Data.Maybe

import Misc

data DunTile = Wall | Room | Hall
  deriving Eq

dunTile a b c = go
  where
    go Wall = a
    go Room = b
    go Hall = c

type Dungeon = Matrix DunTile


cmdLineRender :: Dungeon -> IO ()
cmdLineRender = putStrLn . unlines
              . fmap (unwords . (fmap $ dunTile "." "#" "x")) . toLists

dunGen :: Int -> Int -> Int -> IO Dungeon
dunGen gas sizeX sizeY = go gas $ matrix sizeY sizeX $ const Wall
  where
    sizeMin = 3
    sizeMax = 10

    go :: Int -> Dungeon -> IO Dungeon
    go n dun
      | n <= 0   = return dun
      | otherwise = do
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
        let coord = fromMatCoord p in
        if coord `inRect`
          ((roomX, roomY), (roomX + roomSizeX -1, roomY + roomSizeY -1))
        then Just Room else
        if coord `inRect`
          ((roomX -1, roomY -1), (roomX + roomSizeX, roomY + roomSizeY))
        then Just Wall else
        Nothing
