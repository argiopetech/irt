module Math.IRT.Model.Rasch
  ( RaschModel (RaschModel)
  , difficulty
  ) where

import Control.Lens.TH

import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.Model.FourPLM ( FourPLM(..) )
import Math.IRT.Model.Generic


data RaschModel = RaschModel { _difficulty :: !Double
                             } deriving (Show)

$(makeLenses ''RaschModel)

instance Distribution RaschModel where
    cumulative = cumulative . toFourPLM

instance ContDistr RaschModel where
    density    = density . toFourPLM
    quantile _ = error "This shouldn't be needed"

instance DensityDeriv RaschModel where
    densityDeriv = densityDeriv . toFourPLM

instance GenericModel RaschModel where
    fromRasch           = RaschModel
    fromOnePLM          = RaschModel
    fromTwoPLM      _ b = RaschModel b
    fromThreePLM  _ b _ = RaschModel b
    fromFourPLM _ b _ _ = RaschModel b

instance LogLikelihood RaschModel where
    logLikelihood b = logLikelihood b . toFourPLM

toFourPLM :: RaschModel -> FourPLM
toFourPLM (RaschModel sb) = FourPLM 1.0 sb 0.0 1.0
