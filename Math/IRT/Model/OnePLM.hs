module Math.IRT.Model.OnePLM
  ( OnePLM (OnePLM)
  , difficulty
  ) where

import Control.Lens.TH

import Statistics.Distribution

import Math.IRT.Internal.Distribution 
import Math.IRT.Internal.LogLikelihood
import Math.IRT.Model.FourPLM ( FourPLM(..) )
import Math.IRT.Model.Generic


data OnePLM = OnePLM { _difficulty :: !Double
                     } deriving (Show)

$(makeLenses ''OnePLM)

instance Distribution OnePLM where
    cumulative = cumulative . toFourPLM

instance ContDistr OnePLM where
    density    = density . toFourPLM
    quantile _ = error "This shouldn't be needed"

instance DensityDeriv OnePLM where
    densityDeriv = densityDeriv . toFourPLM

instance GenericModel OnePLM where
    fromRasch           = OnePLM
    fromOnePLM          = OnePLM
    fromTwoPLM      _ b = OnePLM b
    fromThreePLM  _ b _ = OnePLM b
    fromFourPLM _ b _ _ = OnePLM b

instance LogLikelihood OnePLM where
    logLikelihood b = logLikelihood b . toFourPLM

toFourPLM :: OnePLM -> FourPLM
toFourPLM (OnePLM sb) = FourPLM 1.7 sb 0.0 1.0
