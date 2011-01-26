module Math.Vector (
    Vector(..),
    Vector2(..),
    Vector3(..),
) where

import Math.Magnitude
import Math.Normalize
import Math.Scalar

-- | Vector class.
class Vector v where
    apply      :: (Scalar -> Scalar -> Scalar) -> v -> v -> v
    dot        :: v -> v -> Scalar
    cross      :: v -> v -> v
    fromScalar :: Scalar -> v

-- | 2-dimensional vector.
data Vector2 = Vector2 Scalar Scalar
               deriving (Eq, Read, Show)

-- | 3-dimensional vector.
data Vector3 = Vector3 Scalar Scalar Scalar
               deriving (Eq, Read, Show)

-- | 'Vector' instance for 'Vector2'.
instance Vector Vector2 where
    apply f (Vector2 x y) (Vector2 x' y')      = Vector2 (f x x') (f y y')
    dot (Vector2 x y) (Vector2 x' y')          = x * x' + y * y'
    cross                                      = undefined
    fromScalar s                               = Vector2 s s   

-- | 'Vector' instance for 'Vector3'.
instance Vector Vector3 where
    apply f (Vector3 x y z) (Vector3 x' y' z') = Vector3 (f x x') (f y y') (f z z')
    dot (Vector3 x y z) (Vector3 x' y' z')     = x * x' + y * y' + z * z'
    cross (Vector3 x y z) (Vector3 x' y' z')   = Vector3 (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')
    fromScalar s                               = Vector3 s s s
 
-- | 'Num' instance for 'Vector2'.
instance Num Vector2 where
    (+) v v'             = apply (+) v v' 
    (-) v v'             = apply (-) v v'
    (*) v v'             = apply (*) v v'
    negate (Vector2 x y) = Vector2 (-x) (-y) 
    
    abs         = undefined
    signum      = undefined
    fromInteger = undefined

-- | 'Num' instance for 'Vector3'.
instance Num Vector3 where
    (+) v v'               = apply (+) v v' 
    (-) v v'               = apply (-) v v'
    (*) v v'               = apply (*) v v'
    negate (Vector3 x y z) = Vector3 (-x) (-y) (-z)

    abs         = undefined
    signum      = undefined
    fromInteger = undefined

-- | 'Magnitude' instance for 'Vector2'.
instance Magnitude Vector2 where
    magnitudeSquared (Vector2 x y) = x * x + y * y 

-- | 'Magnitude' instance for 'Vector3'.
instance Magnitude Vector3 where
    magnitudeSquared (Vector3 x y z) = x * x + y * y + z * z

-- | 'Normalize' instance for 'Vector2'.
instance Normalize Vector2 where
    normalize (Vector2 x y) = Vector2 (x / m) (y / m)
                              where m = magnitude (Vector2 x y)

-- | 'Normalize' instance for 'Vector3'.
instance Normalize Vector3 where
    normalize (Vector3 x y z) = Vector3 (x / m) (y / m) (z / m)
                                where m = magnitude (Vector3 x y z)

