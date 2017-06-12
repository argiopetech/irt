module Math.IRT.MLE.Fenced ( mleEst
                           , DF (..)
                           , steps
                           , thetaEstimate
                           , lower_fence
                           , upper_fence
                           , fence_discrimination
                           , MLEResult (..)
                           ) where

import Control.Lens.TH

import Data.Default.Class

import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.MLE.Internal.Generic
import Math.IRT.Model.Generic


data DF = DF { _steps                :: !Int
             , _thetaEstimate        :: !Double
             , _lower_fence          :: !Double
             , _upper_fence          :: !Double
             , _fence_discrimination :: !Double }

instance Default DF where
  def = DF 10 0.0 (-3.5) 3.5 3.0

$(makeLenses ''DF)


mleEst :: (ContDistr d, DensityDeriv d, LogLikelihood d, GenericModel d) => DF -> [Bool] -> [d] -> MLEResult
mleEst (DF n vTheta lf uf fd) rs params =
    let resp = True : False : rs
        pars = fromThreePLM fd lf 0 : fromThreePLM fd uf 0 : params
    in generic_mleEst resp pars n vTheta
