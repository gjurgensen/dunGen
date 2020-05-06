module CommandLine where

import System.IO
import Data.List
import Data.Char

import Misc

strip p = dropWhile p . dropWhileEnd p
stripSpace = strip isSpace

-- Single line write doesn't necessarily trigger a flush so we need to do that
-- manually for the prompt to show properly
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

putNewline = putStrLn ""

menu items = go
  where
    lenItems = length items
    (prompts, actions) = unzip items

    printMenu = do
      putNewline
      putStr $ unlines $ fmap (\(n, p) -> n ++ ". " ++ p)
             $ zip (show <$> [1..]) $ prompts ++ ["Exit"]

    go env = do
      printMenu
      sel <- promptRead "Selection: "
      let idx = sel - 1
      if idx `inRange` (0, lenItems)
        then if idx == lenItems
               then return env
               else (actions !! idx) env >>= go
        else do
          putStrLn $ "Bad input; please respond with a number 1-"
                     ++ show (lenItems + 1)
          go env
