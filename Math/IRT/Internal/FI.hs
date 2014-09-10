module Math.IRT.Internal.FI where

import Math.IRT.Internal.IRT
import Math.IRT.Internal.BRM

data FisherInfo = FisherInfo { items :: [Double]
                             , test  :: !Double
                             , sem   :: ! Double
                             } deriving (Show)

-- |The observed Fisher Information
fisherInfo_observed :: Double -> [Response] -> [IRTParameters] -> FisherInfo
fisherInfo_observed theta resps params =
    let items = map (\(u, x) -> go u x) $ zip resps params
        test  = sum items
        sem   = sqrt (1 / test)
    in FisherInfo items test sem
    where go u x = negate $ l'' u x theta


-- |The expected Fisher Information
fisherInfo_expected :: Double -> [IRTParameters] -> FisherInfo
fisherInfo_expected theta params =
    let items = map go params
        test  = sum items
        sem   = sqrt (1 / test)
    in FisherInfo items test sem
    where go x =
              let pActual = p  x theta
                  qActual = q  x theta
                  pDer1   = p' x theta
              in (pDer1 ^ 2) / (pActual * qActual)


-- |The first derivative of `l`, whatever `l` is... Once again, the catIrt documentation disappoints.
-- According to catIrt, "u is the response, and x are the parameters."
-- We only implement the MLE route
l' :: Response -> IRTParameters -> Double -> Double
l' u x theta =
    let pActual = p x theta
        qActual = 1 - pActual
        pDer1   = p'  x theta
        pDer2   = p'' x theta
    in (u - pActual) * pDer1
       ---------------------
       / (pActual * qActual)


-- |The second derivative of `l` (same one as in `l'`)
l'' :: Response -> IRTParameters -> Double -> Double
l'' u x theta =
    let pActual = p x theta
        qActual = q x theta
        pDer1   = p'  x theta
        pDer2   = p'' x theta
        der1    = (((-1) / pActual ^ 2) * (pDer1 ^ 2))
                  + (1 / pActual * pDer2)
        der2    = ((1 / qActual ^ 2) * pDer1 ^ 2)
                  + (1 / qActual * pDer2)
    in u * der1 + (negate $ 1 - u) * der2
