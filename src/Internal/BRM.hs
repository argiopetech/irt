module BRM where

newtype IRTParameters = IRTParameters (Double, Double, Double)

-- |I'm not sure what this function is. It is defined here (as in the catIRT library) without a description.
q :: IRTParameters -> Double -> Double
q (IRTParameters (a, b, c)) theta =
    (1 - c) * (exp $ (-a) * (theta - b))
    ------------------------------------
     / (1 + (exp $ (-a) * (theta - b)))

-- |This is p_i(Θ) as described by [the Wikipedia article on IRT](https://en.wikipedia.org/wiki/Item_response_theory#Three_parameter_logistic_model).
p :: IRTParameters -> Double -> Double
p (IRTParameters (a, b, c)) theta =
    c + (              (1 - c)
         -----------------------------------
         / (1 + (exp $ (-a) * (theta - b))))

-- |The manually-derived first derivative of p_i(Θ)
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
