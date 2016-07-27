module Physics (
    radius,
    collide,
    overlap,
    overlapAny,
    torusfy
  ) where

import Datatypes
import UI
import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace
import FRP.Yampa.Event

-- Collisions

radius :: GameObjectType -> GLfloat
radius (Asteroid scale _)   = scale * 0.05 -- TODO make dependent on shape
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

overlapAny :: GameObject -> [GameObject] -> Bool
overlapAny    object        []              = False
overlapAny    object        (other:[])      = overlap object other
overlapAny    object        (other:others)  = (overlap object other) || (overlapAny object others)

collide :: GameObject -> GameObject -> Event CollisionResult
collide    object        other = case (gameObjectType object, gameObjectType other) of
    (Asteroid _ _, Asteroid _ _) -> collideAsteroids object other
    (_           , _           ) -> explodeObjects object other

explodeObjects :: GameObject -> GameObject -> Event CollisionResult
explodeObjects    object        other
    | overlap object other = Event (Explosion (Circle center size))
    | otherwise            = NoEvent where
        difference = location object ^-^ location other
        center = location object ^+^ ((-0.5) *^ difference)
        size = 0.1


collideAsteroids :: GameObject -> GameObject -> Event CollisionResult
collideAsteroids object other
    | overlap object other = Event (Correction (objectCollisionCorrection , otherCollisionCorrection))
    | otherwise            = NoEvent where
        -- calculate collision normal
        difference = location object ^-^ location other
        collisionNormal = FRP.Yampa.VectorSpace.normalize difference

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
        v1PostCollision = v2Colliding ^-^ v1
        v2PostCollision = v1Colliding ^-^ v2

        -- add the remaining velocities not involved in the collision
        deltaV1 = v1PostCollision ^+^ v1Remaining
        deltaV2 = v2PostCollision ^+^ v2Remaining

        -- calculate the location correction
        distance = norm difference
        radiusSum = radius (gameObjectType object) + radius (gameObjectType other)
        correction = radiusSum - distance
        deltaL1 =    correction *^ collisionNormal
        deltaL2 = (-correction) *^ collisionNormal

        objectCollisionCorrection = CollisionCorrection deltaL1 deltaV1 
        otherCollisionCorrection = CollisionCorrection deltaL2 deltaV2

torusfy :: Location -> Location
torusfy    (Vector x y)
    | x < -1.1 = torusfy (Vector (x + 2.2) y)
    | x >  1.1 = torusfy (Vector (x - 2.2) y)
    | y < -1.1 = torusfy (Vector x (y + 2.2))
    | y >  1.1 = torusfy (Vector x (y - 2.2))
    | otherwise = Vector x y



-- Alternate Approach following the Yampa Arcade Paper: 

--  Game
--
--data ObjectInput = ObjectInput{
--    iHit :: Event (),
--    iUserInput :: UserInput,
--    iCollisionCorrection :: CollisionCorrection
--}
--
--data ObjectOutput = ObjectOutput{
--    gameObject :: GameObject,
--    killEvent :: Event (),
--    spawnEvent :: Event [GameObject]
--}
--
--type Object = SF ObjectInput ObjectOutput
