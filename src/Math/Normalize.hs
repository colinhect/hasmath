module Math.Normalize (
    Normalize(..)
) where

class Normalize a where
    normalize :: a -> a

