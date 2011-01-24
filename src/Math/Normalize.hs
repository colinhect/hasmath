module Math.Normalize where

import Math.Scalar

class Normalize a where
    normalize :: a -> Scalar

