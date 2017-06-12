module Math.IRT.Fisher
    ( FisherInfo(..)
    , fisherInfoObserved
    , fisherInfoExpected
    , l'
    , l''
    ) where

import Statistics.Distribution
import Math.IRT.Internal.Distribution


data FisherInfo = FisherInfo { items :: [Double]
                             , test  :: !Double
                             , sem   :: !Double
                             } deriving (Show)


-- |The observed Fisher Information
--  Applies a min/max prior to ensure a real-valued SEM
fisherInfoObserved :: (ContDistr d, DensityDeriv d) => Double -> [Bool] -> [d] -> FisherInfo
fisherInfoObserved theta resps params =
    let is = zipWith go resps params
        t  = sum is
        s  = sqrt (1 / t)
    in FisherInfo is t s
    where go u x = negate $ l'' u x theta


-- |The expected Fisher Information
fisherInfoExpected :: ContDistr d => Double -> [d] -> FisherInfo
fisherInfoExpected theta params =
    let is = map go params
        t  = sum is
        s  = sqrt (1 / t)
    in FisherInfo is t s
    where go x = let (cdf, ccdf, pdf) = pqDers x theta
                 in (square pdf) / (cdf * ccdf)


-- |The first derivative of `l`, whatever `l` is... Once again, the catIrt documentation disappoints.
-- According to catIrt, "u is the response, and x are the parameters."
-- We only implement the MLE route
l' :: ContDistr d => Bool -> d -> Double -> Double
l' u x theta =
    let (cdf, ccdf, pdf) = pqDers x theta
        denom = cdf * ccdf
        f k   = k * pdf / denom
    in case u of
         True  -> f ccdf
         False -> f cdf


-- |The second derivative of `l` (same one as in `l'`)
l'' :: (ContDistr d, DensityDeriv d) => Bool -> d -> Double -> Double
l'' u x theta =
    let (cdf, ccdf, pdf) = pqDers x theta
        pdf'             = densityDeriv x theta
    in case u of
         True  -> (((-1) / square cdf) * (square pdf))
                  + (1 / cdf * pdf')
         False -> negate $ ((1 / square ccdf) * square pdf)
                           + (1 / ccdf * pdf')


pqDers :: ContDistr d => d -> Double -> (Double, Double, Double)
pqDers x theta = let pComp = cumulative x theta
                     p'    = density x theta
                 in ( pComp
                    , (1 - pComp)
                    , p')

square :: Double -> Double
square = (^^ (2 :: Int))
