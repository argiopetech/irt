module Math.IRT.MLE.Fenced ( mleEst
                           , DF (..)
                           , MLEResult (..)
                           ) where

import Data.Default.Class

import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.MLE.Internal
import Math.IRT.Model.Generic


data DF = DF { steps                :: Int
             , thetaEstimate        :: Double
             , lower_fence          :: Double
             , upper_fence          :: Double
             , fence_discrimination :: Double }

instance Default DF where
  def = DF 10 0.0 (-3.5) 3.5 3.0

mleEst :: (Distribution d, ContDistr d, DensityDeriv d, LogLikelihood d, GenericModel d) => DF -> [Bool] -> [d] -> MLEResult
mleEst (DF n theta lf uf fd) rs params =
    let resp = True : False : rs
        pars = fromThreePLM fd lf 0 : fromThreePLM fd uf 0 : params
    in generic_mleEst resp pars n theta
