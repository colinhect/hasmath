module Math.Quaternion (
    Quaternion,
    conjugate,
    fromAxisAngle
) where

import Math.Angle
import Math.Identity
import Math.Inverse
import Math.Magnitude
import Math.Normalize
import Math.Scalar
import Math.Vector

-- | 'Quaternion' data type.
data Quaternion = Quaternion Scalar Vector3
                  deriving (Eq, Read, Show)

-- | 'Identity' instance for 'Quaternion'.
instance Identity Quaternion where
    identity = Quaternion 1 (Vector3 0 0 0)

-- | 'Inverse' instance for 'Quaternion'.
instance Inverse Quaternion where
    inverse (Quaternion w (Vector3 x y z)) = Quaternion (w / m) (-(Vector3 (x / m) (y / m) (z / m)))
                                             where m = magnitude (Quaternion w (Vector3 x y z))

-- | 'Magnitude' instance for 'Quaternion'.
instance Magnitude Quaternion where
    magnitudeSquared (Quaternion w v) = w * w + magnitudeSquared v

-- | 'Normalize' instance for 'Quaternion'.
instance Normalize Quaternion where
    normalize (Quaternion w (Vector3 x y z)) = Quaternion (w / m) (Vector3 (x / m) (y / m) (z / m))
                                               where m = magnitude (Quaternion w (Vector3 x y z))

-- | 'Num' instance for 'Quaternion'.
instance Num Quaternion where
    (+)                               = undefined
    (-)                               = undefined
    Quaternion w v * Quaternion w' v' = Quaternion (w * w' - dot v v') (v' * fromScalar w + v * fromScalar w' + cross v v')

    abs         = undefined
    signum      = undefined
    fromInteger = undefined

-- | Returns the conjugate of a 'Quaternion'.
conjugate :: Quaternion -> Quaternion
conjugate (Quaternion w v) = Quaternion w (-v)

-- | Builds a 'Quaternion' given an axis of rotation and a rotation angle.
fromAxisAngle :: Vector3 -> Angle -> Quaternion
fromAxisAngle v a = Quaternion (cos theta) (v * fromScalar (sin (theta / 2.0)))
                    where theta = radians a

