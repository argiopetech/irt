{-# LANGUAGE GADTs #-}
module Math.IRT.Model.FourPLM
  ( FourPLM (..)
  ) where

import Numeric.AD (Mode, Scalar, auto)
import Numeric.AD.Mode.Forward.Double
import qualified Numeric.AD.Mode.Tower as T

import Statistics.Distribution

import Math.IRT.Internal.Distribution
import Math.IRT.Internal.LogLikelihood
import Math.IRT.Model.Generic


data FourPLM = FourPLM { discrimination :: !Double
                       , difficulty     :: !Double
                       , pseudoGuessing :: !Double
                       , asymptote      :: !Double
                       } deriving (Show)

instance Distribution FourPLM where
    cumulative = cumulative4PL

instance ContDistr FourPLM where
    density  x = diff (cumulative4PL x)
    quantile _ = error "This shouldn't be needed"

instance DensityDeriv FourPLM where
    densityDeriv x = (!! 2) . T.diffs (cumulative4PL x)

instance GenericModel FourPLM where
    fromRasch         b = FourPLM 1.0 b 0.0 1.0
    fromOnePLM        b = FourPLM 1.7 b 0.0 1.0
    fromTwoPLM      a b = FourPLM   a b 0.0 1.0
    fromThreePLM  a b c = FourPLM   a b   c 1.0
    fromFourPLM         = FourPLM

instance LogLikelihood FourPLM where
    logLikelihood = logLikeFunc cumulative4PL


cumulative4PL :: (Mode a, Floating a, Scalar a ~ Double) =>
       FourPLM -- ^The IRT parameters
    -> a       -- ^Theta
    -> a
cumulative4PL params theta =
    let (a, b, c, d) = makeParamAutos params in
    c + (              (d - c)
         -----------------------------------
         / (1 + exp ((-a) * (theta - b))))
    where makeParamAutos (FourPLM sa sb sc sd) =
              let a = auto sa
                  b = auto sb
                  c = auto sc
                  d = auto sd
              in (a, b, c, d)
