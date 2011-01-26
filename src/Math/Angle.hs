module Math.Angle (
    Angle(..),
    radians
) where

import Math.Scalar

-- | 'Angle' data type.
data Angle = Radians Scalar
           | Degrees Scalar
             deriving (Eq, Read, Show)

-- | Returns the value in radians of an 'Angle'.
radians :: Angle -> Scalar
radians (Radians r) = r
radians (Degrees d) = (pi / 180.0) * d

