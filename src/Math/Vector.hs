module Math.Vector (

) where

import Math.Normalize
import Math.Scalar

data Vector2 = Vector2 Scalar Scalar Scalar
               deriving (Eq, Read, Show)

data Vector3 = Vector3 Scalar Scalar Scalar
               deriving (Eq, Read, Show)


