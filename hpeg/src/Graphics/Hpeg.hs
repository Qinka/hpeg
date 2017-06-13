{-|
Module      : Graphics.Hpeg
Description : Translate between bmp and hpeg
Copyright   : (C) Qinka 2017
License     : GPL-3
Maintainer  : qinka@live.com
Stability   : experimental
Portability : unknow
-}


{-# LANGUAGE RecordWildCards #-}

module Graphics.Hpeg
       ( bmp2hpeg
       , hpeg2bmp
       ) where

import           Control.Monad
import           Data.Word
import           Graphics.Hpeg.Color
import           Graphics.Hpeg.DCT
import           Graphics.Hpeg.Matrix
import           Graphics.Hpeg.Store

import qualified Data.Array.Accelerate as A

hpeg2bmp :: FilePath -> FilePath
         -> (A.Acc (A.Array A.DIM2 Word32)
                -> (A.Array A.DIM2 Word32))
         -> IO ()
hpeg2bmp fp ofp runAcc = do
  HpegStore{..} <- readHpeg fp
  let ys  = A.use $ fromBk $ hData !! 0
      cbs = A.use $ fromBk $ hData !! 1
      crs = A.use $ fromBk $ hData !! 2
      fromBk it = fromArray $ fromBlocks it uSize vSize
      yi = idct2 ys  (lumaCoe8 :: A.Acc (A.Array A.DIM2 Float))
      bi = idct2 cbs (chromaCoe8 :: A.Acc (A.Array A.DIM2 Float))
      ri = idct2 crs (chromaCoe8 :: A.Acc (A.Array A.DIM2 Float))
      r = toRed   yi    ri
      g = toGreen yi bi ri
      b = toBlue  yi bi
      rgb = combineFromTri r g b
      real = runAcc rgb
  writeImageToBMP ofp real

bmp2hpeg :: FilePath -> FilePath
         -> (A.Acc (A.Array A.DIM2 Word16)
                -> (A.Array A.DIM2 Word16))
         -> IO ()
bmp2hpeg fp ofp runAcc = do
  bmpdata <- readImageFromBMP fp
  case bmpdata of
    Left e -> error $ show e
    Right bmp -> void $ do
      let (r,g,b,_) = splitToTri $ A.use bmp
          y  = toY  r g b
          cb = toCb r g b
          cr = toCr r g b
          yd = dct2  y (lumaCoe8   :: A.Acc (A.Array A.DIM2 Float))
          bd = dct2 cb (chromaCoe8 :: A.Acc (A.Array A.DIM2 Float))
          rd = dct2 cr (chromaCoe8 :: A.Acc (A.Array A.DIM2 Float))
          ys  = runAcc yd
          cbs = runAcc bd
          crs = runAcc rd
      writeHpeg ofp $ toHpegStore ys cbs crs
  where toHpegStore ys cbs crs =
          let toBk = toBlocks . toArray
              (rtr,u,v) = toBk ys
              (rtg,_,_) = toBk cbs
              (rtb,_,_) = toBk crs
          in HpegStore u v $ rtr:rtg:rtb:[]

