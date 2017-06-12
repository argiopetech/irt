module Math.IRT.Model.TwoPLM
  ( TwoPLM (..)
  ) where

import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.Model.FourPLM ( FourPLM(..) )
import Math.IRT.Model.Generic


data TwoPLM = TwoPLM { discrimination :: !Double
                     , difficulty     :: !Double
                     } deriving (Show)

instance Distribution TwoPLM where
    cumulative = cumulative . toFourPLM

instance ContDistr TwoPLM where
    density    = density . toFourPLM
    quantile _ = error "This shouldn't be needed"

instance DensityDeriv TwoPLM where
    densityDeriv = densityDeriv . toFourPLM

instance GenericModel TwoPLM where
    fromRasch         b = TwoPLM 1.0 b
    fromOnePLM        b = TwoPLM 1.7 b
    fromTwoPLM          = TwoPLM
    fromThreePLM  a b _ = TwoPLM   a b
    fromFourPLM a b _ _ = TwoPLM   a b

instance LogLikelihood TwoPLM where
    logLikelihood b = logLikelihood b . toFourPLM

toFourPLM :: TwoPLM -> FourPLM
toFourPLM (TwoPLM sa sb) = FourPLM sa sb 0.0 1.0
