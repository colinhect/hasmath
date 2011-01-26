module Matrix (
    Matrix(..)
) where

import Math.Identity
import Math.Scalar

data Matrix = Matrix [Scalar]
              deriving (Eq, Read, Show)

instance Identity Matrix where
    identity = Matrix [1, 0, 0, 0,
                       0, 1, 0, 0,
                       0, 0, 1, 0,
                       0, 0, 0, 1]

