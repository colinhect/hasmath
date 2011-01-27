module Matrix (
    Matrix(..),
    translate
) where

import Math.Identity
import Math.Scalar
import Math.Vector

-- | 'Matrix' data type.
data Matrix = Matrix [Scalar]
              deriving (Eq, Read, Show)

-- | 'Identity' instance for 'Matrix'.
instance Identity Matrix where
    identity = Matrix [1, 0, 0, 0,
                       0, 1, 0, 0,
                       0, 0, 1, 0,
                       0, 0, 0, 1]

-- | Translates the given 'Matrix' by the given 'Vector3'.
translate :: Matrix -> Vector3 -> Matrix
translate (Matrix cs) (Vector3 x y z) = Matrix (take 12 cs ++ (x' : y' : z' : [last cs]))
                                        where x' = x + cs !! 12
                                              y' = y + cs !! 13
                                              z' = z + cs !! 14

-- | Builds a transformation matrix given the translation and rotation.
transformation :: Vector3 -> Quaternion -> Matrix
transformation = undefined
