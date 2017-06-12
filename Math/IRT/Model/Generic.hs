module Math.IRT.Model.Generic where

class GenericModel a where
    fromRasch    :: Double -> a
    fromOnePLM   :: Double -> a
    fromTwoPLM   :: Double -> Double -> a
    fromThreePLM :: Double -> Double -> Double -> a
    fromFourPLM  :: Double -> Double -> Double -> Double -> a
