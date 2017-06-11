{-# LANGUAGE GADTs #-}
module Math.IRT.MLE.Internal ( MLEResult (..)
                             , generic_mleEst
                             , logLike
                             ) where

import Numeric.AD (Mode, Scalar)
import Numeric.AD.Halley

import Statistics.Distribution (Distribution, ContDistr)

import Math.IRT.Fisher
import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood


data MLEResult = MLEResult { theta :: !Double
                           , info  :: !Double
                           , sem   :: !Double
                           } deriving (Show)


generic_mleEst :: (Distribution d, ContDistr d, DensityDeriv d, LogLikelihood d) => [Bool] -> [d] -> Int -> Double -> MLEResult
generic_mleEst rs params steps theta =
    let est    = (!! steps) $ extremumNoEq (logLike rs params) theta
        fisher = fisherInfoObserved est rs params
    in case fisher of
         (FisherInfo _ test sem) -> MLEResult est test sem


logLike :: (Mode a, Floating a, Scalar a ~ Double, Distribution d, LogLikelihood d) => [Bool] -> [d] -> a -> a
logLike responses params theta =
    let logLik   = zipWith (\a b -> logLikelihood a b theta) responses params
        bmePrior = 1
    in sum logLik + log bmePrior
