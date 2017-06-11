{-|
Module      : Graphics.Hpeg.Store
Description : Store the hpeg and bmp.
Copyright   : (C) Qinka 2017
License     : GPL-3
Maintainer  : qinka@live.com
Stability   : experimental
Portability : unknow
-}
{-# LANGUAGE RecordWildCards #-}

module  Graphics.Hpeg.Store
        ( HpegStore(..)
        , readHpeg
        , writeHpeg
        ) where

import           Control.Monad
import           Data.Binary     (Binary, get, getWord8, put)
import qualified Data.Binary     as Bin
import           Data.ByteString ()
import qualified Data.ByteString as B
import           Data.Word


data HpegStore = HpegStore { uSize :: !Word32
                           , vSize :: !Word32
                           , hData :: [[[(Word8,Word8)]]]
                           }
instance Binary HpegStore where
  put HpegStore{..} = do
    put ((0x48,0x70,0x45,0x67) :: (Word8,Word8,Word8,Word8))
    put uSize
    put vSize
    put hData
  get = do
    h <- getWord8
    p <- getWord8
    e <- getWord8
    g <- getWord8
    case (h,p,e,g) of
      (0x48,0x70,0x45,0x67) -> liftM3 HpegStore get get get
      _                     -> error "error magic number"


readHpeg :: FilePath -> IO HpegStore
readHpeg = Bin.decodeFile
writeHpeg :: FilePath -> HpegStore -> IO ()
writeHpeg = Bin.encodeFile
