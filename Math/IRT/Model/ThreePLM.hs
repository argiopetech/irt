module Math.IRT.Model.ThreePLM
  ( ThreePLM (..)
  ) where

import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.Model.FourPLM ( FourPLM(..) )
import Math.IRT.Model.Generic


data ThreePLM = ThreePLM { discrimination :: !Double
                         , difficulty     :: !Double
                         , pseudoGuessing :: !Double
                         } deriving (Show)

instance Distribution ThreePLM where
    cumulative = cumulative . toFourPLM

instance ContDistr ThreePLM where
    density    = density . toFourPLM
    quantile _ = error "This shouldn't be needed"

instance DensityDeriv ThreePLM where
    densityDeriv = densityDeriv . toFourPLM

instance GenericModel ThreePLM where
    fromRasch         b = ThreePLM 1.0 b 0.0
    fromOnePLM        b = ThreePLM 1.7 b 0.0
    fromTwoPLM      a b = ThreePLM   a b 0.0
    fromThreePLM        = ThreePLM
    fromFourPLM a b c _ = ThreePLM   a b   c

instance LogLikelihood ThreePLM where
    logLikelihood b = logLikelihood b . toFourPLM

toFourPLM :: ThreePLM -> FourPLM
toFourPLM (ThreePLM sa sb sc) = FourPLM sa sb sc 1.0
