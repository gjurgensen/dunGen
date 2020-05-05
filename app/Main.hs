module Main where

import System.Environment
import System.Directory
import Control.Monad.Extra
import Data.List
import Data.Char

import Dungeon
import Image

data Env = Env {getDunParams :: (Int, Int, Int),
                getDun       :: Dungeon}

strip p = dropWhile p . dropWhileEnd p
stripSpace = strip isSpace

cmdLineDun:: IO Env
cmdLineDun = do
  putStrLn "Grid width: "
  x <- readLn
  putStrLn "Grid height: "
  y <- readLn
  putStrLn "Grid density: "
  gas <- readLn
  putStrLn "Generating..."
  dun <- dunGen gas x y
  putStrLn "Done."
  return $ Env (gas, x, y) dun

-- TODO: abstract menu/option process. type [(String, IO ())]
menu :: Env -> IO ()
menu env = printMenu >> readLn >>= doSel
 where
  printMenu = putStrLn $ unlines
    ["1. Preview in terminal",
     "2. Render image",
     "3. Regenerate with same parameters",
     "4. Regererate with new parameters",
     "5. Exit",
     "Selection: "]

  doSel 1 = cmdLineRender (getDun env) >> menu env
  doSel 2 = do
    putStrLn "Filename: "
    file   <- stripSpace <$> getLine
    exists <- doesFileExist file
    when exists $ do
      putStrLn $ file ++ " already exists. Overwrite it? (y/n)"
      yes <- getYN
      when (not yes) $ menu env
    err <- genImage file (getDun env)
    whenJust err $ putStrLn . ("Error: " ++)
    menu env
  doSel 3 = do
    putStrLn "Generating..."
    let (gas, x, y) = getDunParams env
    dun <- dunGen gas x y
    putStrLn "Done."
    menu $ Env (gas, x, y) dun
  doSel 4 = cmdLineDun >>= menu
  doSel 5 = return ()
  doSel _ = putStrLn "Bad input; please respond with a number 1-5."
            >> menu env

  getYN :: IO Bool
  getYN = do
    str <- stripSpace <$> getLine
    let strUpper = toUpper <$> str
    case str of
      "Y" -> return True
      "N" -> return False
      _   -> putStrLn "Bad input; please respond with \"y\" or \"n\"."
             >> getYN

main = cmdLineDun >>= menu
