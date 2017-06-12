module Math.IRT.MLE
  ( DF (..)
  , MLEResult (..)
  , mleEst
  ) where

import Data.Default.Class

import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.MLE.Internal.Generic


data DF = DF { steps         :: !Int
             , thetaEstimate :: !Double }

instance Default DF where
  def = DF 10 0.0


mleEst :: (Distribution d, ContDistr d, DensityDeriv d, LogLikelihood d) => DF -> [Bool] -> [d] -> MLEResult
mleEst (DF n vTheta) rs params =
    generic_mleEst rs params n vTheta
