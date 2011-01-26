module Math.Magnitude (
    Magnitude(..)
) where

import Math.Scalar

class Magnitude a where
    magnitudeSquared :: a -> Scalar
    magnitude        :: a -> Scalar
    magnitude        = sqrt . magnitudeSquared

