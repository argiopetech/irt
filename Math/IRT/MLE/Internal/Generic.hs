{-# LANGUAGE GADTs #-}
module Math.IRT.MLE.Internal.Generic
  ( MLEResult (..)
  , generic_mleEst
  , logLike
  ) where

import Numeric.AD (Mode, Scalar)
import Numeric.AD.Halley

import Statistics.Distribution (Distribution, ContDistr)

import Math.IRT.Fisher
import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.MLE.Internal.MLEResult


generic_mleEst :: (Distribution d, ContDistr d, DensityDeriv d, LogLikelihood d) => [Bool] -> [d] -> Int -> Double -> MLEResult
generic_mleEst rs params steps vTheta =
    let est    = (!! steps) $ extremumNoEq (logLike rs params) vTheta
        fisher = fisherInfoObserved est rs params
    in case fisher of
         (FisherInfo _ t s) -> MLEResult est t s


logLike :: (Mode a, Floating a, Scalar a ~ Double, Distribution d, LogLikelihood d) => [Bool] -> [d] -> a -> a
logLike responses params vTheta =
    let logLik   = zipWith (\a b -> logLikelihood a b vTheta) responses params
        bmePrior = 1
    in sum logLik + log bmePrior
