module Math.IRT.Model.Generic where

class GenericModel a where
    fromRaschLogistic :: Double -> a
    fromRaschNormal   :: Double -> a
    fromTwoPLM        :: Double -> Double -> a
    fromThreePLM      :: Double -> Double -> Double -> a
    fromFourPLM       :: Double -> Double -> Double -> Double -> a
