module Data.Direction.Class where

class Orientation a where
    azimuth :: a -> Double
    -- | @fromAzimuth (azimuth a) = a@
    fromAzimuth :: Double -> a
    opposite :: a -> a