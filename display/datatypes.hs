module Datatypes (
  Vector(..),
  GameObjectType(..),
  GameObject(..)
  ) where

data Vector = Vector {
  x :: Double,
  y :: Double
  } deriving (Eq, Show)


data GameObjectType = Ship | EnemyShip | Asteroid
  deriving (Eq, Show)

data GameObject = GameObject {
  location :: Vector,
  velocity :: Vector,
  orientation :: Double,
  gameObjectType :: GameObjectType
  }