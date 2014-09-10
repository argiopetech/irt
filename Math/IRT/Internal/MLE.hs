module Math.IRT.Internal.MLE 
    ( MleEst(..)
    , mleEst
    ) where

import Numeric.AD.Halley

import Math.IRT.Internal.BRM
import Math.IRT.Internal.IRT
import Math.IRT.Internal.FI

data MleEst = MleEst { theta :: !Double
                     , info  :: !Double
                     , sem   :: !Double
                     } deriving (Show)

-- |Estimate the maximum likelihood estimate of θ using the Binary Response Model
mleEst :: [Response] -> [IrtParameters] -> MleEst
mleEst resp params =
    let est    = last $ extremum (logLike resp params) 0
        fisher = fisherInfoObserved est resp params
    in case fisher of
         (FisherInfo _ test sem) -> MleEst est test sem

-- Tests
{-
plotData = do
    let range = [-6, -5.99 .. 6]
        points = zip range $ map (runId . logLike testResponses testIRTParams) range
        string = unlines $ map (\(x, y) -> show x ++ ' ':show y) points
    writeFile "plotPoints.out" string
-}

testIrtParams = map IrtParameters
                [ (1, -0.0664, 0)
                , (1, -2.4939, 0)
                , (1, -1.2971, 0)
                , (1, -2.1392, 0)]

testResponses = [0.0, 1.0, 0.0, 1.0]

testPoints = map IrtParameters
             [ (1, -1.7207, 0)
             , (1, -2.0625, 0)
             , (1, -1.7512, 0)
             , (1, -2.0594, 0)]
