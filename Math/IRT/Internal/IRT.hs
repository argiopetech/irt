module Math.IRT.Internal.IRT
    ( IrtParameters(..)
    , Response
    ) where

type Response = Double
newtype IrtParameters = IrtParameters (Double, Double, Double)
