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
dunGen gas sizeX sizeY = 
  let center = ((sizeX -1) `div` 2, (sizeY -1) `div` 2) in
  tunnel [(gas, center, U), (gas, center, R),
          (gas, center, D), (gas, center, L)]
       $ matrix sizeY sizeX $ const Wall
  where
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
