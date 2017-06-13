{-|
Module      : Graphics.Hpeg.Matrix
Description : The methods about matrix
Copyright   : (C) Qinka, 2017
License     : GPL-3
Maintainer  : qinka@live.com
Stability   : experimental
Portability : unknow

The methods about the matrix to translate/form.
-}

{-# LANGUAGE FlexibleContexts #-}

module Graphics.Hpeg.Matrix
       ( toBlocks
       , fromBlocks
       , toArray
       , fromArray
       , splitToTri
       , combineFromTri
       , AIO.readImageFromBMP
       , AIO.writeImageToBMP
       ) where


import           Data.Array               as Array
import           Data.Array.Accelerate    ((:.) (..), Acc, DIM2, Exp, Z (..))
import qualified Data.Array.Accelerate    as A
import qualified Data.Array.Accelerate.IO as AIO
import           Data.Bits
import           Data.Word                (Word16, Word32, Word8)
import qualified Data.Word                as Word

-- | Create the index of the block
mkBlockIndex :: Word16 -> Word16 -> [(Word16,Word16)]
mkBlockIndex u v = [(a,b) | a <- [1..u], b <- [1..v]]

mkBlock :: Array (Word32,Word32) Word16 -> (Word16,Word16) -> [(Word16,Word16)]
mkBlock arr (bi,bj)  = filter ((/=0).snd) $ map trans items
  where items = [(i,j,arr ! mkIndex bi bj i j) | i <- [1..8], j <- [1..8], check i j]
        trans (i',j',x') = let i = fromIntegral i'
                               j = fromIntegral j'
                               x = fromIntegral x'
                           in (i*16+j,x)
        (u,v) = maximum $ indices arr
        check i j = fromIntegral (bi-1) * 8 + fromIntegral i <= u
                 && fromIntegral (bj-1) * 8 + fromIntegral j <= v
toBlocks :: Array (Word32,Word32) Word16 -> ([[(Word16,Word16)]],Word32,Word32)
toBlocks arr = (map (mkBlock arr) indexs,u,v)
  where (u,v) = maximum $ indices arr
        blockSize var = fromIntegral $ div var 8 + 1
        indexs = mkBlockIndex (blockSize u) (blockSize v)


mkIndex :: Word16 -> Word16 -> Word8 -> Word8 -> (Word32,Word32)
mkIndex bi' bj' i' j' = (bi*8+i,bj*8+j)
  where bi = fromIntegral bi' - 1
        bj = fromIntegral bj' - 1
        i  = fromIntegral i'
        j  = fromIntegral j'

fromBlocks :: [[(Word16,Word16)]] -> Word32 -> Word32 -> Array (Word32,Word32) Word16
fromBlocks its u v = array ((1,1),(u,v)) $ zeros ++ arr
  where p = fromIntegral $ div u 8 + 1
        q = fromIntegral $ div v 8 + 1
        zeros  = [((a,b),0) | a <- [1..u], b <-[1..v]]
        indexs = mkBlockIndex p q
        arr = concat $ zipWith (\it (p,q) -> map (kmBlock (fromIntegral p,fromIntegral q)) it) its indexs


kmBlock :: (Word32,Word32) -> (Word16,Word16) -> ((Word32,Word32),Word16)
kmBlock (bi,bj) (ij,x) = (((bi-1)*8+i,(bj-1)*8+j),x)
  where i = fromIntegral $ div ij 16
        j = fromIntegral $ mod ij 16


fromArray :: Array (Word32,Word32) Word16 -> A.Array DIM2 Word16
fromArray arr =A.fromList (Z :. fromIntegral u :. fromIntegral v) $ elems arr
  where (u,v) = maximum $ indices arr

toArray :: A.Array DIM2 Word16 -> Array (Word32,Word32) Word16
toArray arr = array ((1,1),(u,v)) $ zipWith (\i x -> (i,x)) indexs list
  where (Z :. u' :. v') = A.arrayShape arr
        u = fromIntegral u'
        v = fromIntegral v'
        list = A.toList arr
        indexs = [(i,j) | i <- [1..u], j <- [1..v]]


splitToTri :: Acc (A.Array DIM2 Word32)
              -- ^                R                        G                        B                        A
           -> (Acc (A.Array DIM2 Word8),Acc (A.Array DIM2 Word8),Acc (A.Array DIM2 Word8),Acc (A.Array DIM2 Word8))
splitToTri = A.unzip4 . A.map rgb
  where rgb :: Exp Word32 -> Exp (Word8,Word8,Word8,Word8)
        rgb i = let ddiv p q = (A.fromIntegral $ p `mod` q,p `div` q)
                    (b,b') = ddiv i  256
                    (g,g') = ddiv b' 256
                    (r,r') = ddiv g' 256
                    (a,a') = ddiv r' 256
                in A.lift (r,g,b,a)

combineFromTri :: Acc (A.Array DIM2 Word8)  -- ^ R
               -> Acc (A.Array DIM2 Word8)  -- ^ G
               -> Acc (A.Array DIM2 Word8)  -- ^ B
               -> Acc (A.Array DIM2 Word32) -- ^ RGBA
combineFromTri rs gs bs = A.zipWith3 rgb rs gs bs
  where rgb r' g' b' = let r = A.fromIntegral r'
                           g = A.fromIntegral g'
                           b = A.fromIntegral b'
                           a = 255
                       in ((a*256 + r) *  256 + g) * 256 + b

