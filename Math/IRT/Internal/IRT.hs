module Math.IRT.Internal.IRT
    ( IrtParameters(..)
    , Response
    ) where

type Response = Double
data IrtParameters = IrtParameters { discrimination :: Double
                                   , difficulty     :: Double
                                   , pseudoGuessing :: Double
                                   } deriving (Show)
