module Math.IRT.Internal.Distribution where

class DensityDeriv a where
    densityDeriv :: a -> Double -> Double
