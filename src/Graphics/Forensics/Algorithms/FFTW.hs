module Graphics.Forensics.Algorithms.FFTW
       ( dft1d
       , dft2d
       , dft3d
       , idft1d
       , idft2d
       , idft3d
       , dct1d
       , dct2d
       , dct3d
       , idct1d
       , idct2d
       , idct3d
       ) where

import Data.Array.Repa (Array(..), DIM1, DIM2, DIM3)
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Complex
import Math.FFT as FFTW

import Graphics.Forensics.Utilities (r2c1d, r2c2d, r2c3d, liftC)

{- Normalized discrete fourier transform and inverse -}
dft1d :: Array F DIM1 (Complex Double) -> Array F DIM1 (Complex Double)
dft1d = liftC r2c1d $ FFTW.dftN [0]

dft2d :: Array F DIM2 (Complex Double) -> Array F DIM2 (Complex Double)
dft2d = liftC r2c2d $ FFTW.dftN [0,1]

dft3d :: Array F DIM3 (Complex Double) -> Array F DIM3 (Complex Double)
dft3d = liftC r2c3d $ FFTW.dftN [0,1,2]

idft1d :: Array F DIM1 (Complex Double) -> Array F DIM1 (Complex Double)
idft1d = liftC r2c1d $ FFTW.idftN [0]

idft2d :: Array F DIM2 (Complex Double) -> Array F DIM2 (Complex Double)
idft2d = liftC r2c2d $ FFTW.idftN [0,1]

idft3d :: Array F DIM3 (Complex Double) -> Array F DIM3 (Complex Double)
idft3d = liftC r2c3d $ FFTW.idftN [0,1,2]

{- Type II discrete cosine transform and inverse -}
dct1d :: Array F DIM1 Double -> Array F DIM1 Double
dct1d = liftC r2c1d $ FFTW.dct2N [0]

dct2d :: Array F DIM2 Double -> Array F DIM2 Double
dct2d = liftC r2c2d $ FFTW.dct2N [0,1]

dct3d :: Array F DIM3 Double -> Array F DIM3 Double
dct3d = liftC r2c3d $ FFTW.dct2N [0,1,2]

idct1d :: Array F DIM1 Double -> Array F DIM1 Double
idct1d = liftC r2c1d $ FFTW.dct3N [0]

idct2d :: Array F DIM2 Double -> Array F DIM2 Double
idct2d = liftC r2c2d $ FFTW.dct3N [0,1]

idct3d :: Array F DIM3 Double -> Array F DIM3 Double
idct3d = liftC r2c3d $ FFTW.dct3N [0,1,2]