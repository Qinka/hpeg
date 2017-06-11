module Main where

import           Data.Array.Accelerate.Interpreter
import           Graphics.Hpeg
import           System.Environment
main :: IO ()
main = do
  pg <- takeWhile (/= '.') <$> getProgName
  from:to:_ <- getArgs
  case pg of
    "bmp2hpeg" -> bmp2hpeg from to run
    "hpeg2bmp" -> hpeg2bmp from to run