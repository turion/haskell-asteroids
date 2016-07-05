module Physics (
    CollisionCorrection(..),
    radius,
    collide
  ) where

import Datatypes
import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace
import FRP.Yampa.Event

data CollisionCorrection = CollisionCorrection {
  deltaLocation :: Location,
  deltaVelocity :: Velocity
}

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



-- TODO? move objects back in old direction according to the "time" they've been overlapping
-- and instead in the new direction according to that same time and the new velocities
collide :: GameObject -> GameObject -> (Event CollisionCorrection, Event CollisionCorrection)
collide object other 
    | overlap object other = (Event objectCollisionCorrection , Event otherCollisionCorrection)
    | otherwise            = (NoEvent, NoEvent) where
        -- calculate collision normal
        difference = location object ^-^ location other
        collisionNormal = (1 / norm difference) *^ difference

        -- calculate parts of v1 that collide and the remainder
        v1 = velocity object
        v1Dot = dot collisionNormal v1
        v1Colliding = v1Dot *^ collisionNormal
        v1Remaining = v1 ^-^ v1Colliding

        -- calculate parts of v2 that collide and the remainder
        v2 = velocity other
        v2Dot = dot collisionNormal v2
        v2Colliding = v2Dot *^ collisionNormal
        v2Remaining = v2 ^-^ v2Colliding

        -- calculate results of the actually colliding parts via an inelastic collision
        v1PostCollision = v2Colliding ^-^ v1Colliding
        v2PostCollision = v1Colliding ^-^ v2Colliding

        -- add the remaining velocities not involved in the collision
        deltaV1 = v1PostCollision ^+^ v1Remaining
        deltaV2 = v2PostCollision ^+^ v2Remaining

        -- calculate the location correction
        distance = norm difference
        radiusSum = radius (gameObjectType object) + radius (gameObjectType other)
        correction = distance - radiusSum
        deltaL1 = (-0.01 - correction) *^ v1
        deltaL2 = (-0.01 - correction) *^ v2

        objectCollisionCorrection = CollisionCorrection deltaL1 deltaV1 
        otherCollisionCorrection = CollisionCorrection deltaL2 deltaV2 
