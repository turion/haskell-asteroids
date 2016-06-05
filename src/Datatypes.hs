module Datatypes (
  Ship
  ) where

data Vector = Vector {
  x :: Double,
  y :: Double
  } deriving (Eq, Show)


class Located a where
  getLocation :: a -> Vector

class Oriented a where
  getOrientation :: a -> Vector

class (Located a, Oriented a) => Displayable a where 
  draw :: a -> IO()

-- SHIP --

data Ship = Ship {
  sLocation :: Vector,
  sVelocity :: Vector,
  sOrientation :: Vector
  } deriving (Eq, Show)

instance Located Ship where
  getLocation = sLocation

instance Oriented Ship where
  getOrientation = sOrientation

instance Displayable Ship where
  draw ship = return ()


-- ASTEROID --

data Asteroid = Asteroid {
  aLocation :: Vector,
  aVelocity :: Vector,
  aOrientation :: Vector
}

instance Located Asteroid where
  getLocation = aLocation

instance Oriented Asteroid where
  getOrientation = aOrientation

instance Displayable Asteroid where
  draw asteroid = return ()