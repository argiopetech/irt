module Math.IRT.Fisher
    ( FisherInfo(..)
    , fisherInfoObserved
    , fisherInfoExpected
    ) where

import Statistics.Distribution
import Math.IRT.Internal.Distribution


data FisherInfo = FisherInfo { items :: [Double]
                             , test  :: !Double
                             , sem   :: !Double
                             } deriving (Show)

-- |The observed Fisher Information
--  Applies a min/max prior to ensure a real-valued SEM
fisherInfoObserved :: (Distribution d, ContDistr d, DensityDeriv d) => Double -> [Bool] -> [d] -> FisherInfo
fisherInfoObserved theta resps params =
    let items = zipWith go resps params
        test  = sum items
        sem   = sqrt (1 / test)
    in FisherInfo items test sem
    where go u x = negate $ l'' u x theta


-- |The expected Fisher Information
--fisherInfoExpected :: (Distribution d, ContDistr d) => Double -> [d] -> FisherInfo
fisherInfoExpected theta params =
    let items = map go params
        test  = sum items
        sem   = sqrt (1 / test)
    in FisherInfo items test sem
    where go x = let (cdf, ccdf, pdf, _) = pqDers x theta
                 in (pdf ^ 2) / (cdf * ccdf)


-- |The first derivative of `l`, whatever `l` is... Once again, the catIrt documentation disappoints.
-- According to catIrt, "u is the response, and x are the parameters."
-- We only implement the MLE route
l' :: (Distribution d, ContDistr d, DensityDeriv d) => Bool -> d -> Double -> Double
l' u x theta =
    let (cdf, ccdf, pdf, _) = pqDers x theta
        denom = cdf * ccdf
        f x   = x * pdf / denom
    in case u of
         True  -> f ccdf
         False -> f cdf


-- |The second derivative of `l` (same one as in `l'`)
l'' :: (Distribution d, ContDistr d, DensityDeriv d) => Bool -> d -> Double -> Double
l'' u x theta =
    let (cdf, ccdf, pdf, pdf') = pqDers x theta
    in case u of
         True  -> (((-1) / cdf ^ 2) * (pdf ^ 2))
                  + (1 / cdf * pdf')
         False -> negate $ ((1 / ccdf ^ 2) * pdf ^ 2)
                           + (1 / ccdf * pdf')


pqDers :: (Distribution d, ContDistr d, DensityDeriv d) => d -> Double -> (Double, Double, Double, Double)
pqDers x theta = let pComp = cumulative x theta
                     p'    = density x theta
                     p''   = densityDeriv x theta
                 in ( pComp
                    , (1 - pComp)
                    , p'
                    , p'')
