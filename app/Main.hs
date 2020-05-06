module Main where

import System.Directory
import Control.Monad

import CommandLine
import Dungeon
import Image


data Env = Env {getDunParams :: (Int, Int, Int),
                getDun       :: Dungeon}


fillEnv :: IO Env
fillEnv = do
  x   <- promptRead "Grid width: "
  y   <- promptRead "Grid height: "
  gas <- promptRead "Density: "
  putStrLn "Generating..."
  dun <- dunGen gas x y
  putStrLn "Done."
  return $ Env (gas, x, y) dun


dunMenu = menu [("Preview in terminal", preview),
                ("Render image", render),
                ("Regenerate with same parameters", regenSame),
                ("Regenerate with new parameters", regenNew)]
  where
    preview env = do
      cmdLineRender $ getDun env
      return env

    render env = do
      file   <- prompt "Filename: "
      exists <- doesFileExist file
      if not exists
        then renderTo file
        else do
          yes <- promptYN $ file ++ " already exists. Overwrite it? (y/n): "
          when yes $ renderTo file
      return env
        where
          renderTo file = do
            putStrLn "Rendering..."
            err <- genImage file (getDun env)
            putStrLn $ maybe "Done." ("Error: " ++) err

    regenSame env = do
      putStrLn "Generating..."
      let (gas, x, y) = getDunParams env
      dun <- dunGen gas x y
      putStrLn "Done."
      return $ Env (gas, x, y) dun

    regenNew _ = fillEnv
  

main = fillEnv >>= dunMenu
