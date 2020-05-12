module Dungeon where

import System.Random
import Control.Monad
--import Control.Applicative
--import Control.Type.Operator
import Data.Bool
import Data.Matrix
import Data.Maybe
--import Data.Function

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


data Dir = U | R | D | L

dir a b c d = go
  where
    go U = a
    go R = b
    go D = c
    go L = d

dirToVec = dir (0,-1) (1,0) (0,1) (-1,0)

rotClockwise = dir R D L U
rotCounter   = dir L U R D

type Worm = (Int, Coord, Dir)

dunGen :: Int -> Int -> Int -> IO Dungeon
dunGen gas sizeX sizeY = do
  let center = ((sizeX -1) `div` 2, (sizeY -1) `div` 2)
  let initWorms = [(gas, center, U), (gas, center, R),
                   (gas, center, D), (gas, center, L)]
  tunneled <- tunnel initWorms $ matrix sizeY sizeX $ const Wall
  placeRooms gas tunneled
  where
    sizeMin = 3
    sizeMax = 10

    placeRooms :: Int -> Dungeon -> IO Dungeon
    placeRooms n dun
      | n <= 0    = return dun
      | otherwise = do
          room <- genRoom
          if or  (elementwise collision    room dun) ||
             and (elementwise disconnected room dun)
          then placeRooms (n-1) dun
          else placeRooms n $ combine room dun
      where
        collision    ma b = isJust ma && b == Room
        disconnected ma b = case (ma, b) of
          (Just Room, Hall) -> False
          _ -> True
        combine = elementwise $ \ma b -> case ma of 
          Just Room -> Room
          _ -> b

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

    tunnel :: [Worm] -> Dungeon -> IO Dungeon
    tunnel []    dun = return dun
    tunnel worms dun = do
      worms1 <- join <$> (sequence $ plan <$> worms)
      let worms2 = act <$> worms1
      tunnel worms2 $ foldr dig dun worms2

    plan :: Worm -> IO [Worm]
    plan (life, coord, dir) =
      if life == 0 then
        return mzero
      else do
        let worm = (life -1, coord, dir)
        r <- randomRIO (0,29) :: IO Int
        --return $ case r of
        --  0 -> [worm, rotWormClockwise worm]
        --  1 -> [worm, rotWormCounter   worm]
        --  2 -> [rotWormClockwise worm]
        --  3 -> [rotWormCounter   worm]
        --  _ -> [worm]
        return $ if r < 1 then [worm, rotWormClockwise worm] else
                 if r < 2 then [worm, rotWormCounter   worm] else
                 if r < 4 then [rotWormClockwise worm] else
                 if r < 6 then [rotWormCounter   worm] else
                 [worm]
      where
        rotWormClockwise (l, c, dir) = (l, c, rotClockwise dir)
        rotWormCounter   (l, c, dir) = (l, c, rotCounter   dir)

    act (life, coord, dir) = (life, move dir coord, dir)
         
    dig (_, (x,y), _) dun = adjSetElem dun x y Hall

    move :: Dir -> Coord -> Coord
    move dir coord = restrictRect (0,0) (sizeX -1,sizeY -1)
                   $ liftPair2 (+) coord $ dirToVec dir
