module Math.IRT.Internal.MLE 
    ( MleEst(..)
    , mleEst
    ) where

import Numeric.AD.Halley
import Numeric.AD.Internal.Identity

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
    let est    = last $ take 15 $ extremum (logLike resp params) 0
        fisher = fisherInfoObserved est resp params
    in case fisher of
         (FisherInfo _ test sem) -> MleEst est test sem

-- Tests

plotData :: IO ()
plotData = do
    let range = [-6, -5.99 .. 6]
        points = zip (map runId range) $ map (runId . logLike testResponses testIrtParams) range
        string = unlines $ map (\(x, y) -> show x ++ ' ':show y) points
    writeFile "plotPoints.out" string

testResponses = [0.0,1.0,0.0,1.0]
testIrtParams = [IrtParameters {discrimination = 1.0, difficulty = 0.7387, pseudoGuessing = 0.0},IrtParameters {discrimination = 1.0, difficulty = -6.64e-2, pseudoGuessing = 0.0},IrtParameters {discrimination = 1.0, difficulty = 0.0, pseudoGuessing = 0.0},IrtParameters {discrimination = 1.0, difficulty = 0.0, pseudoGuessing = 0.0}]

{-
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
-}
