module Math.IRT.MLE.Truncated ( mleEst
                              , DF (..)
                              , MLEResult (..)
                              , steps
                              , thetaEstimate
                              , lower_bound
                              , upper_bound
                              ) where

import Control.Lens.TH

import Data.Default.Class

import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.MLE.Internal.Generic


data DF = DF { _steps         :: !Int
             , _thetaEstimate :: !Double
             , _lower_bound   :: !Double
             , _upper_bound   :: !Double }

instance Default DF where
  def = DF 10 0.0 (-3.5) 3.5

$(makeLenses ''DF)


mleEst :: (Distribution d, ContDistr d, DensityDeriv d, LogLikelihood d) => DF -> [Bool] -> [d] -> MLEResult
mleEst (DF n th lb ub) rs params =
    let res = generic_mleEst rs params n th
    in res { theta = min ub $ max lb $ theta res }
