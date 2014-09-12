{-# LANGUAGE GADTs #-}
module Math.IRT.Internal.BRM 
    ( logLike
      -- *p_i(θ) and its derivatives
    , p
    , p'
    , p''
      -- *q
    , q
    ) where

import Numeric.AD
import Numeric.AD.Halley
import Numeric.AD.Internal.Identity

import Math.IRT.Internal.IRT

makeParamAutos (IrtParameters sa sb sc) =
    let a = auto sa
        b = auto sb
        c = auto sc
    in (a, b, c)

-- |q is equivalent to (1 - p), It is defined here (as in the catIRT library) using a much more complicated algorithm (which notably calls exp twice vs. 'p's once).
--
-- It would be entirely valid (albeit potentially inefficient) to implement this as
--
-- > q params theta = 1 - p params theta
q :: (Mode s, Floating s, Scalar s ~ Double) => IrtParameters -> s -> s
q params theta =
    let (a, b, c) = makeParamAutos params in
    (1 - c) * exp ((-a) * (theta - b))
    ------------------------------------
     / (1 + exp ((-a) * (theta - b)))


-- |This is p_i(Θ) as described by <https://en.wikipedia.org/wiki/Item_response_theory#Three_parameter_logistic_model the Wikipedia article on IRT>.
p :: (Mode a, Floating a, Scalar a ~ Double) => IrtParameters
  -> a -- ^Theta
  -> a
p params theta =
    let (a, b, c) = makeParamAutos params in
    c + (              (1 - c)
         -----------------------------------
         / (1 + exp ((-a) * (theta - b))))


-- |The manually-derived first derivative of p_i(θ)
--
-- I briefly derived this via AD, but, since they were already derived in catIrt, it wasn't worth the performance overhead, additional complexity, and pulling in AD.
p' :: IrtParameters -> Double -> Double
p' (IrtParameters a b c) theta =
    let ex = exp $ a * (theta - b)
    in (1 - c) * a * ex
       ----------------
        / (1 + ex) ^ 2


-- |The manually-derived second derivative of p_i(Θ)
--
-- As with the first derivative, this was better pre-derived than as an AD-derived function.
p'' :: IrtParameters -> Double -> Double
p'' (IrtParameters a b c) theta =
    let ex = exp $ a * (theta - b)
    in (1 - c) * (a ^ 2) * (ex - (ex ^ 2))
       -----------------------------------
                / ((1 + ex) ^ 3)


-- |Calculates the log likelihood (of what?)
--
-- Only implements MLE, so bmePrior is 1
logLike :: (Mode a, Floating a, Scalar a ~ Double) => [Response] -> [IrtParameters] -> a -> a
logLike sus xs theta =
    let -- us       = map auto sus
        pActuals = map (`p` theta) xs
--        logLik   = zipWith3 (\(u, pActual, qActual) -> (auto u * log pActual) + ((1 - auto u) * log qActual)) sus pActuals qActuals
        logLik = map (\(u, pActual) -> (u * log pActual) + ((1 - u) * log (1 - pActual))) $ zip (map auto sus) pActuals
        bmePrior = 1
    in sum logLik + log bmePrior
