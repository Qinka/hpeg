module Main where

import           Backend
import           Graphics.Hpeg
import           System.Environment
main :: IO ()
main = do
  pg <- takeWhile (/= '.') <$> getProgName
  from:to:_ <- getArgs
  case pg of
    "bmp2hpeg" -> bmp2hpeg from to run
    "hpeg2bmp" -> hpeg2bmp from to run
    _          -> putStrLn "Error program name"
