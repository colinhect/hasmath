module Math.Inverse (
    Inverse(..)
) where

class Inverse a where
    inverse :: a -> a

