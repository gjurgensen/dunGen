module Main where

import System.Environment
import Text.Read

import Matrix
import Map

main = do
  args <- getArgs
  case sequence $ readMaybe <$> args of
    Just [gas, x, y] -> (genMap gas x y) >>=
                        (render . (fmap $ mapElem ' ' '#' 'x'))
    _ -> print "Bad!"
