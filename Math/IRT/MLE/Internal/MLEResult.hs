module Math.IRT.MLE.Internal.MLEResult where

data MLEResult = MLEResult { theta :: !Double
                           , info  :: !Double
                           , sem   :: !Double
                           } deriving (Show)
