module Math.IRT.Internal.FI
    ( FisherInfo(..)
    , fisherInfoObserved
    , fisherInfoExpected
    ) where

import Math.IRT.Internal.IRT
import Math.IRT.Internal.BRM

data FisherInfo = FisherInfo { items :: [Double]
                             , test  :: !Double
                             , sem   :: ! Double
                             } deriving (Show)

-- |The observed Fisher Information
--  Applies a min/max prior to ensure a real-valued SEM
fisherInfoObserved :: Double -> [Response] -> [IrtParameters] -> FisherInfo
fisherInfoObserved theta resps params =
    let items = zipWith go resps params
        test  = sum items
        sem   = sqrt (1 / test)
    in FisherInfo items test sem
    where go u x = negate $ l'' u x theta


-- |The expected Fisher Information
fisherInfoExpected :: Double -> [IrtParameters] -> FisherInfo
fisherInfoExpected theta params =
    let items = map go params
        test  = sum items
        sem   = sqrt (1 / test)
    in FisherInfo items test sem
    where go x =
              let (pActual, qActual, pDer1, _) = pqDers x theta
              in (pDer1 ^ 2) / (pActual * qActual)


-- |The first derivative of `l`, whatever `l` is... Once again, the catIrt documentation disappoints.
-- According to catIrt, "u is the response, and x are the parameters."
-- We only implement the MLE route
l' :: Response -> IrtParameters -> Double -> Double
l' u x theta =
    let (pActual, qActual, pDer1, _) = pqDers x theta
    in (u - pActual) * pDer1
       ---------------------
       / (pActual * qActual)


-- |The second derivative of `l` (same one as in `l'`)
l'' :: Response -> IrtParameters -> Double -> Double
l'' u x theta =
    let (pActual, qActual, pDer1, pDer2) = pqDers x theta
        der1    = (((-1) / pActual ^ 2) * (pDer1 ^ 2))
                  + (1 / pActual * pDer2)
        der2    = ((1 / qActual ^ 2) * pDer1 ^ 2)
                  + (1 / qActual * pDer2)
    in u * der1 + negate (1 - u) * der2


pqDers :: IrtParameters -> Double -> (Double, Double, Double, Double)
pqDers x theta = let pComp = p x theta
                 in ( pComp
                    , (1 - pComp)
                    , p'  x theta
                    , p'' x theta)
