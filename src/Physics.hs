module Physics (
    radius,
    collide
  ) where

import Datatypes
import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace
import FRP.Yampa.Event

radius :: GameObjectType -> GLfloat
radius (Asteroid scale)     = scale * 0.05
radius Ship                 = 0.05
radius EnemyShip            = 0.05
radius Projectile           = 0.02
radius EnemyProjectile      = 0.02

overlap :: GameObject -> GameObject -> Bool
overlap    object        other         
    | object == other = False
    | otherwise       = distance <= (r1 + r2) where
        distance = norm $ loc1 ^-^ loc2
        loc1 = location object
        loc2 = location other
        r1 = radius $ gameObjectType object
        r2 = radius $ gameObjectType other

--getOverlappingObjects :: GameObject -> [GameObject] -> Maybe [GameObject]
--getOverlappingObjects object [] = Nothing
--getOverlappingObjects object (other:others) 
--    | overlap object other = Just $ other : (getOverlappingObjects object others)
--    | otherwise            = getOverlappingObjects object others

collide :: GameObject -> GameObject -> (Event CollisionCorrection, Event CollisionCorrection)
collide object other 
    | overlap object other = (NoEvent, NoEvent)
    | otherwise            = (NoEvent, NoEvent)
