{-# LANGUAGE ViewPatterns, DeriveDataTypeable #-}
module Data.Direction.Four where
import Data.Direction.Class
import Data.Fixed
import Data.Typeable

data Direction = East | North | West | South deriving (Show, Read, Eq, Ord, Enum, Typeable)

instance Orientation Direction where
    azimuth East = 0
    azimuth North = pi / 2
    azimuth West = pi
    azimuth South = 3 * pi / 2
    fromAzimuth ((/(pi/2)) . (`mod'` 4) -> t)
      | t <= 0.5 = East
      | t <= 1.5 = North
      | t <= 2.5 = West
      | t <= 3.5 = South
      | otherwise = East
    opposite East = West
    opposite North = South
    opposite West = East
    opposite South = North