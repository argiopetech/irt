module MLE where

import Numeric.AD
import Numeric.AD.Halley

import BRM
import IRT
import FI

data MleEst = MleEst { theta :: !Double
                     , info  :: !Double
                     , sem   :: !Double -- ^For the moment, this avoids a name clash
                                        -- Eventually, it can be handled with a new module
                     } deriving (Show)

-- |Estimate the maximum likelihood estimate of θ using the Binary Response Model
mleEst :: [Response] -> [IRTParameters] -> MleEst
mleEst resp params =
    let est    = last $ extremum (logLike resp params) 0
        fisher = fisherInfo_observed est resp params
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

testIRTParams = map IRTParameters
                [ (1, -0.0664, 0)
                , (1, -2.4939, 0)
                , (1, -1.2971, 0)
                , (1, -2.1392, 0)]

testResponses = [0.0, 1.0, 0.0, 1.0]

testPoints = map IRTParameters
             [ (1, -1.7207, 0)
             , (1, -2.0625, 0)
             , (1, -1.7512, 0)
             , (1, -2.0594, 0)]
