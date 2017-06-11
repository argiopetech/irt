{-# LANGUAGE GADTs #-}
module Math.IRT.Internal.LogLikelihood where

import Numeric.AD (Mode, Scalar)
import Statistics.Distribution (Distribution, cumulative)

class (Distribution d) => LogLikelihood d where
    logLikelihood :: (Mode a, Floating a, Scalar a ~ Double) => Bool -> d -> a -> a

logLikeFunc :: (Distribution d, Mode a, Floating a, Scalar a ~ Double) => (d -> a -> a) -> Bool -> d -> a -> a
logLikeFunc f True  = (log .) . f
logLikeFunc f False = ((log . (1-)) .) . f
