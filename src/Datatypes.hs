module Datatypes (
  Vector(..),
  GameObjectType(..),
  GameObject(..),
  Location,
  Velocity,
  Acceleration,
  Orientation
  ) where

data Vector = Vector {
  getX :: Double,
  getY :: Double
  } deriving (Eq, Show)

type Location = Vector
type Velocity = Vector
type Acceleration = Double
type Orientation = Double

data GameObjectType = Ship | Asteroid 
  deriving Show

data GameObject = GameObject {
  getLocation :: Location,
  getVelocity :: Velocity,
  getOrientation :: Orientation,
  getGameObjectType :: GameObjectType
  }


