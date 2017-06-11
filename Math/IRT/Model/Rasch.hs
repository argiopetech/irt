module Math.IRT.Model.Rasch ( RaschModel (RaschModel)
                            , difficulty
                            ) where

import Control.Lens.TH

import Numeric.AD
import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.Model.FourPLM ( FourPLM(..) )
import Math.IRT.Model.Generic
import Math.IRT.Model.Rasch.Internal


instance Distribution RaschModel where
    cumulative = cumulative . toFourPLM

instance ContDistr RaschModel where
    density    = density . toFourPLM
    quantile _ = error "This shouldn't be needed"

instance DensityDeriv RaschModel where
    densityDeriv = densityDeriv . toFourPLM

instance GenericModel RaschModel where
    fromRaschLogistic   = RaschModel
    fromRaschNormal     = RaschModel
    fromTwoPLM      _ b = RaschModel b
    fromThreePLM  _ b _ = RaschModel b
    fromFourPLM _ b _ _ = RaschModel b

instance LogLikelihood RaschModel where
    logLikelihood b = logLikelihood b . toFourPLM

toFourPLM (RaschModel sb) = FourPLM 1.7 sb 0.0 1.0
