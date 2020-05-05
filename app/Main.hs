module Main where

import System.Environment
import System.Directory
import System.IO
import Control.Monad
import Data.List
import Data.Char

import Dungeon
import Image

data Env = Env {getDunParams :: (Int, Int, Int),
                getDun       :: Dungeon}

strip p = dropWhile p . dropWhileEnd p
stripSpace = strip isSpace

-- Single line write doesn't necessarily trigger a flush so we need to do that
-- automatically for the prompt to show
prompt:: String -> IO String
prompt str = do
  putStr str
  hFlush stdout
  stripSpace <$> getLine

-- TODO: make fault tolerant with readMaybe
promptRead :: Read a => String -> IO a
promptRead str = do
  putStr str
  hFlush stdout
  readLn

promptYN str = do
  putStr str
  hFlush stdout
  inp <- stripSpace <$> getLine
  case toUpper <$> inp of
    "Y" -> return True
    "N" -> return False
    _   -> putStrLn "Bad input; please respond with \"y\" or \"n\"."
           >> promptYN str

cmdLineDun:: IO Env
cmdLineDun = do
  x   <- promptRead "Grid width: "
  y   <- promptRead "Grid height: "
  gas <- promptRead "Grid density: "
  putStrLn "Generating..."
  dun <- dunGen gas x y
  putStrLn "Done."
  return $ Env (gas, x, y) dun

-- TODO: abstract menu/option process. type [(String, IO ())]
menu :: Env -> IO ()
menu env = printMenu >> readLn >>= doSel
 where
  printMenu = putStr $ unlines
    ["",
     "1. Preview in terminal",
     "2. Render image",
     "3. Regenerate with same parameters",
     "4. Regererate with new parameters",
     "5. Exit",
     "Selection: "]

  doSel 1 = cmdLineRender (getDun env) >> menu env
  doSel 2 = do
    putStr "Filename: "
    file   <- stripSpace <$> getLine
    exists <- doesFileExist file
    when exists $ do
      yes <- promptYN $ file ++ " already exists. Overwrite it? (y/n): "
      when (not yes) $ menu env
    putStrLn "Rendering..."
    err <- genImage file (getDun env)
    putStrLn $ maybe "Done." ("Error: " ++) err
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
  
main = cmdLineDun >>= menu
