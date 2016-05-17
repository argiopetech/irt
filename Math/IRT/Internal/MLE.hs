module Math.IRT.Internal.MLE 
    ( MleEst(..)
    , mleEst
    , mleEst'
    ) where

import Numeric.AD.Halley

import Math.IRT.Internal.BRM
import Math.IRT.Internal.IRT
import Math.IRT.Internal.FI

data MleEst = MleEst { theta :: !Double
                     , info  :: !Double
                     , sem   :: !Double
                     } deriving (Show)

-- |Runs mleEst' with depth = 10 and theta = 0.
--
-- This is usually sufficient for starting a chain, but may search to an unnecessary depth on non-trivially convergent likelihoods once a reasonable approximation to theta has been established.
mleEst :: [Bool] -> [IrtParameters] -> MleEst
mleEst resp params = mleEst' resp params 10 0

-- |Estimate the maximum likelihood estimate of θ using the Binary Response Model
mleEst' :: [Bool] -> [IrtParameters] -> Int -> Double -> MleEst
mleEst' rs params n θ =
    let resp   = map boolToDouble rs
        est    = minMaxPrior $ last $ take n $ extremum (logLike resp params) θ
        fisher = fisherInfoObserved est resp params
    in case fisher of
         (FisherInfo _ test sem) -> MleEst est test sem
    where boolToDouble True  = 1.0
          boolToDouble False = 0.0
          minMaxPrior = let bound = 12
                        in min bound . max (negate bound)
