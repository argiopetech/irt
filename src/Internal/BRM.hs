{-# LANGUAGE GADTs #-}
module BRM where

import Numeric.AD
import Numeric.AD.Halley
import Numeric.AD.Internal.Identity

import IRT

-- |I'm not sure what this function is. It is defined here (as in the catIrt library) without a description.
q :: (Mode s, Floating s, Scalar s ~ Double) => IRTParameters -> s -> s
q (IRTParameters (sa, sb, sc)) theta =
    let a = auto sa
        b = auto sb
        c = auto sc in
    (1 - c) * (exp $ (-a) * (theta - b))
    ------------------------------------
     / (1 + (exp $ (-a) * (theta - b)))


-- |This is p_i(Θ) as described by [the Wikipedia article on IRT](https://en.wikipedia.org/wiki/Item_response_theory#Three_parameter_logistic_model).
-- p :: (Mode a, Floating a, Scalar a ~ Double) => IRTParameters -> a -> a
p :: (Mode a, Floating a, Scalar a ~ Double) => IRTParameters -> a -> a
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


-- |Calculates the log likelihood (of?)
-- Only implements MLE, so bmePrior is 1
logLike :: (Mode a, Floating a, Scalar a ~ Double) => [Response] -> [IRTParameters] -> a -> a
logLike sus xs theta =
    let us       = map auto sus
        pActuals = map (\x -> p x theta) xs
        qActuals = map (\x -> q x theta) xs
        logLik   = map (\(u, pActual, qActual) -> (u * log pActual) + ((1 - u) * log qActual)) $ zip3 us pActuals qActuals
        bmePrior = 1
    in sum logLik + log bmePrior
