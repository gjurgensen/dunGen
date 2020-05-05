module Main where

import System.Environment
import Text.Read

import Dungeon
import Image

main = do
  args <- getArgs
  case sequence $ readMaybe <$> args of
    Just [gas, x, y] -> dunGen gas x y
                    >>= genImage "dungeon.bmp"
                    >>= maybe (print "Image saved") (print . ("Error: " ++))
    _ -> print "Bad!"
