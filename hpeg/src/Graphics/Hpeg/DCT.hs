{-|
Module      : Graphics.Hpeg.DCT
Description : The DCT and IDCT for images.
Copyright   : (C) Qinka 2017
License     : GPL-3
Maintainer  : qinka@live.com
Stability   : experimental
Portability : Unknow

The DCT and IDCT.
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Graphics.Hpeg.DCT
       ( -- * real dct  methods
         realdct28
       , realidct28
       , -- * dct with quantifing
         dct2
       , idct2
       , -- * coefficient
         lumaCoe8
       , chromaCoe8
       ) where

import           Data.Array.Accelerate ((:.) (..), Acc, All (..), Array, DIM1,
                                        DIM2, DIM3, DIM4, Elt, Exp, Shape,
                                        ToFloating, Word16, Word8, Z (..), (!),
                                        (?|))
import qualified Data.Array.Accelerate as A

-- | realdct2 for 2D-DCT
realdct28 :: (Elt e,A.Num e,A.Floating e,ToFloating Int e,A.Eq e)
         => Acc (Array DIM2 e) -- ^ The input matrix
         -> Acc (Array DIM2 e) -- ^ Output one
realdct28 arr = A.zipWith (*) cs $ A.fold (+) 0 $ A.fold (+) 0 $ A.zipWith5 com is js us vs ar
  where Z :. rowM :. colN = A.unlift (A.shape arr) :: Z :. Exp Int :. Exp Int
        com :: (Elt e,A.Floating e,ToFloating Int e) => Exp e -> Exp e -> Exp e -> Exp e -> Exp e -> Exp e
        com i j u v f =  A.cos ((2*i+1)*u*A.pi/16) * A.cos((2*j+1)*v*A.pi/16) * f
        bu = A.generate  (A.lift $ Z :. rowM) $ \i -> A.toFloating $ A.unindex1 i `A.mod` 8
        bv = A.generate  (A.lift $ Z :. colN) $ \i -> A.toFloating $ A.unindex1 i `A.mod` 8
        us = A.replicate (A.lift $ Z :. All  :. colN :. eigh :. eigh) bu
        vs = A.replicate (A.lift $ Z :. rowM :. All  :. eigh :. eigh) bv
        is = A.generate  (A.lift $ Z :. rowM :. colN :. eigh :. eigh) mkI
        js = A.generate  (A.lift $ Z :. rowM :. colN :. eigh :. eigh) mkJ
        ar = A.generate  (A.lift $ Z :. rowM :. colN :. eigh :. eigh) (mkAr arr)
        cs = A.generate  (A.lift $ Z :. rowM :. colN) mkCoe

-- | idct2 for 2D-IDCT
realidct28 :: (Elt e,A.Num e,A.Floating e,ToFloating Int e,A.Eq e)
           => Acc (Array DIM2 e) -- ^ Input
           -> Acc (Array DIM2 e) -- ^ Output
realidct28 arr = A.fold (+) 0 $ A.fold (+) 0 $ A.zipWith6 icom cs is js us vs ar
  where Z :. rowM :. colN = A.unlift (A.shape arr) :: Z :. Exp Int :. Exp Int
        icom :: (Elt e,A.Floating e,ToFloating Int e) => Exp e -> Exp e -> Exp e -> Exp e -> Exp e -> Exp e -> Exp e
        icom c i j u v f = c * A.cos ((2*i+1)*u*A.pi/16) * A.cos((2*j+1)*v*A.pi/16) * f -- A.cos (i*(2*u+1)*A.pi/16) * A.cos(j*(2*v+1)*A.pi/16) * f
        bu = A.generate  (A.lift $ Z :. rowM) $ \i -> A.toFloating $ A.unindex1 i `A.mod` 8
        bv = A.generate  (A.lift $ Z :. colN) $ \i -> A.toFloating $ A.unindex1 i `A.mod` 8
        us = A.generate  (A.lift $ Z :. rowM :. colN :. eigh :. eigh) mkI
        vs = A.generate  (A.lift $ Z :. rowM :. colN :. eigh :. eigh) mkJ
        is = A.replicate (A.lift $ Z :. All  :. colN :. eigh :. eigh) bu
        js = A.replicate (A.lift $ Z :. rowM :. All  :. eigh :. eigh) bv
        ar = A.generate  (A.lift $ Z :. rowM :. colN :. eigh :. eigh) (mkAr arr)
        cs = A.replicate (A.lift $ Z :. rowM :. colN :. All  :. All ) $
             A.generate  (A.lift $ Z :. eigh :. eigh) mkCoe
-- | Make I
mkI :: (ToFloating Int e,Elt e,A.Floating e,A.Num e)
    => Exp DIM4 -- ^ Sharpe
    -> Exp e    -- ^ result
mkI sh = let Z :. _ :. _ :. c :. _ = A.unlift sh :: Z :. Exp Int :. Exp Int :. Exp Int :. Exp Int
         in A.toFloating $ A.mod c 8

-- | Make J
mkJ :: (ToFloating Int e,Elt e,A.Floating e,A.Num e)
    => Exp DIM4 -- ^ Sharpe
    -> Exp e
mkJ sh = let Z :. _ :. _ :. _ :. d = A.unlift sh :: Z :. Exp Int :. Exp Int :. Exp Int :. Exp Int
         in A.toFloating $ A.mod d 8

-- | Make array
mkAr :: (Elt e,A.Num e)
     => Acc (Array DIM2 e) -- ^ arr
     -> Exp DIM4       -- ^ shape
     -> Exp e
mkAr ar sh = let Z :. a' :. b' :. c :. d = A.unlift sh :: Z :. Exp Int :. Exp Int :. Exp Int :. Exp Int
                 a = A.div a' 8
                 b = A.div b' 8
                 m = a*8+c
                 n = b*8+d
                 sh' = A.lift $ Z :. m :. n :: Exp DIM2
                 Z :. row :. col = A.unlift (A.shape ar) :: Z :. Exp Int :. Exp Int
             in (m A.< row A.&& n A.< col) A.? (ar ! sh',0)

-- | The method for c(i)
cFunc :: (A.Floating e,A.Eq e)
      => Exp Int -- ^ i
      -> Exp e   -- ^ c(i)
cFunc i' = let i = A.mod i' 8
           in A.ifThenElse (i A.== 0) (A.sqrt 2 / 2) 1

-- | Create 1D array
mk :: (Elt e,A.Floating e,A.ToFloating Int e)
   => Exp Int -- ^ size
   -> Acc (Array DIM1 e) -- array
mk len = A.generate (A.lift $ Z :. len) $ \i' ->
  let i = A.toFloating $ A.unindex1 i' `mod` 8
  in (2 * i + 1) * A.pi / 16

mkCoe :: (ToFloating Int e,Elt e,A.Floating e,A.Eq e)
      => Exp DIM2   -- ^ sharp
      -> Exp e      -- result
mkCoe sh = let Z :. u :. v = A.unlift sh :: Z :. Exp Int :. Exp Int
           in cFunc u * cFunc v / 4

-- | Eight
eigh :: Exp Int
eigh = 8

-- | Float or Double ==>> Word8
roundToWord8 :: A.RealFrac a
             => Acc (Array DIM2 a)       -- ^ Input Float for something else
             -> Acc (Array DIM2 Word8)   -- ^ Output Word8
roundToWord8 = A.map (A.round . A.max (0) . A.min (255))

-- | Word8 ==>> Float or Double
deroundFromWord8 :: (A.Floating e,A.ToFloating Word8 e)
                 => Acc (Array DIM2 Word8) -- ^ Input word8
                 -> Acc (Array DIM2 e)     -- ^ output float or double
deroundFromWord8 = A.map A.toFloating

-- | 8 x 8 matrix's quantifing luma-coefficient table
lumaCoe8 :: (Num a,Elt a)
         => Acc (Array DIM2 a)
lumaCoe8 = A.use $ A.fromList (Z :. 8 :. 8)
  [ 16, 11, 10, 16, 24,  40,  51,  61
  , 12, 12, 14, 19, 26,  58,  60,  55
  , 14, 13, 16, 24, 40,  57,  69,  56
  , 14, 17, 22, 29, 51,  87,  80,  62
  , 18, 22, 37, 56, 68,  109, 103, 77
  , 24, 35, 55, 64, 81,  104, 113, 92
  , 49, 64, 78, 87, 103, 121, 120, 101
  , 72, 92, 95, 98, 112, 100, 103, 99
  ]

-- | 8 x 8 matrix's quantifing  chroma-coefficient table
chromaCoe8 :: (Num a,Elt a)
           => Acc (Array DIM2 a)
chromaCoe8 = A.use $ A.fromList (Z :. 8 :. 8)
  [ 17, 18, 24, 47, 99, 99, 99, 99
  , 18, 21, 26, 66, 99, 99, 99, 99
  , 24, 26, 56, 99, 99, 99, 99, 99
  , 47, 66, 99, 99, 99, 99, 99, 99
  , 99, 99, 99, 99, 99, 99, 99, 99
  , 99, 99, 99, 99, 99, 99, 99, 99
  , 99, 99, 99, 99, 99, 99, 99, 99
  , 99, 99, 99, 99, 99, 99, 99, 99
  ]

-- | dct with quantifing
dct2 :: (A.RealFrac e,ToFloating Word8 e,ToFloating Int e,Elt e,A.Floating e)
     => Acc (Array DIM2 Word8) -- ^ input
     -> Acc (Array DIM2 e)     -- ^ coefficient
     -> Acc (Array DIM2 Word8) -- ^ output
dct2 mat coe' = let matF = A.map (\x -> x - 128) $ deroundFromWord8 mat
                    coe  = mkRealCoe (A.shape mat) coe'
                    dctM = realdct28 matF
                    fin  = A.map (\x -> x + 128) $ A.zipWith (/) dctM coe
                in  A.map (\x -> x - 128) $ roundToWord8 fin
-- | idct with quantifing
idct2 :: (A.RealFrac e,ToFloating Word8 e,ToFloating Int e,Elt e,A.Floating e)
      => Acc (Array DIM2 Word8) -- ^ input
      -> Acc (Array DIM2 e)     -- ^ coefficient
      -> Acc (Array DIM2 Word8) -- ^ output
idct2 mat coe' = let matF = deroundFromWord8 $ A.map (\x -> x + 128) mat
                     matD = A.map (\x -> x - 128) matF
                     matC = A.zipWith (*) matD coe
                     dctM = realidct28 matC
                     coe  = mkRealCoe (A.shape mat) coe'
                 in roundToWord8 $ A.map (\x -> x + 128) dctM

mkRealCoe :: Elt e
          => Exp DIM2
          -> Acc (Array DIM2 e)
          -> Acc (Array DIM2 e)
mkRealCoe sh coe = A.generate sh mki
  where mki s = let Z :. i :. j = A.unlift s :: Z :. Exp Int :. Exp Int
                    Z :. p :. q = A.unlift (A.shape coe) :: Z :. Exp Int :. Exp Int
                in coe ! (A.lift $ Z :. (mod i p) :. (mod j q))
