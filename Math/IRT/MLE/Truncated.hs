module Math.IRT.MLE.Truncated where

import Data.Default.Class

import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.MLE.Internal
import Math.IRT.Model.Generic


data DF = DF { steps         :: Int
             , thetaEstimate :: Double
             , lower_bound   :: Double
             , upper_bound   :: Double }

instance Default DF where
  def = DF 10 0.0 (-3.5) 3.5

mleEst :: (Distribution d, ContDistr d, DensityDeriv d, LogLikelihood d) => DF -> [Bool] -> [d] -> MLEResult
mleEst (DF n th lb ub) rs params =
    let res = generic_mleEst rs params n th
    in res { theta = min ub $ max lb $ theta res }
