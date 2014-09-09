{-# LANGUAGE GADTs #-}
module BRM where

import Numeric.AD
import Numeric.AD.Halley

newtype IRTParameters = IRTParameters (Double, Double, Double)

-- |I'm not sure what this function is. It is defined here (as in the catIrt library) without a description.
--q :: IRTParameters -> Double -> Double
q (IRTParameters (sa, sb, sc)) theta =
    let a = auto sa
        b = auto sb
        c = auto sc in
    (1 - c) * (exp $ (-a) * (theta - b))
    ------------------------------------
     / (1 + (exp $ (-a) * (theta - b)))


-- |This is p_i(Θ) as described by [the Wikipedia article on IRT](https://en.wikipedia.org/wiki/Item_response_theory#Three_parameter_logistic_model).
-- p :: (Mode a, Floating a, Scalar a ~ Double) => IRTParameters -> a -> a
p (IRTParameters (sa, sb, sc)) theta =
    let a = auto sa
        b = auto sb
        c = auto sc in
    c + (              (1 - c)
         -----------------------------------
         / (1 + (exp $ (-a) * (theta - b))))


-- |The manually-derived first derivative of p_i(θ)
-- I briefly derived this via AD, but, since they were already derived in catIrt, it wasn't worth the performance overhead, additional complexity, and pulling in AD.
p' :: IRTParameters -> Double -> Double
p' (IRTParameters (a, b, c)) theta =
    let ex = exp $ a * (theta - b)
    in (1 - c) * a * ex
       ----------------
        / (1 + ex) ^ 2


-- |The manually-derived second derivative of p_i(Θ)
-- As with the first derivative, this was better pre-derived than as an AD-derived function.
p'' :: IRTParameters -> Double -> Double
p'' (IRTParameters (a, b, c)) theta =
    let ex = exp $ a * (theta - b)
    in (1 - c) * (a ^ 2) * (ex - (ex ^ 2))
       -----------------------------------
                / ((1 + ex) ^ 3)


type Response = Double
data FisherInfo = FisherInfo Double Double

-- |The observed Fisher Information
-- catIrt also supports calculating the expected Fisher Information, the that isn't of use at the moment
fisherInfo :: IRTParameters -> Double -> Response -> FisherInfo
fisherInfo params theta resp =
    let info  = negate $ l'' resp params theta
        sem   = sqrt (1 / info)
    in FisherInfo info sem

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


-- |Calculates the log likelihood (of?)
-- Only implements MLE, so bmePrior is 1
--logLike :: Response -> IRTParameters -> Double -> Double
--logLike :: (Mode a, Floating a, Scalar a ~ Double) => Response -> IRTParameters -> a -> a
logLike :: (Mode a, Floating a, Scalar a ~ Double) => Double -> IRTParameters -> a -> a
logLike su x theta =
    let u        = auto su
        pActual  = p x theta
        qActual  = q x theta
        logLik   = (u * log pActual) + ((1 - u) * log qActual)
        bmePrior = 1
    in logLik + log bmePrior

type Range = (Double, Double)
data MleEst = MleEst Double Double Double

-- |Estimate the maximum likelihood estimate of θ using the Binary Response Model
mleEst :: Response -> IRTParameters -> MleEst
mleEst resp params =
    let est    = last $ findZero (logLike resp params) 0
        fisher = fisherInfo params est resp
    in case fisher of
         (FisherInfo test sem) -> MleEst est test sem

{-
  # Then, maximize the loglikelihood function over that interval for each person:
  for( i in 1:dim(resp)[1] ){
    likFun <- paste("logLik.", mod, sep = "")
    est[i] <- optimize( get(likFun), lower = l, upper = u, maximum = TRUE,
                        x = params, u = resp[i, ],
                        type = "MLE" )$max
-}
