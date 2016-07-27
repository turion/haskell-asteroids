module Physics (
    radius,
    collide,
    overlap,
    torusfy
  ) where

import Datatypes
import UI
import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace
import FRP.Yampa.Event

-- Collisions

radius :: GameObjectType -> GLfloat
radius (Asteroid scale _ _)   = scale * 0.05 -- TODO make dependent on shape
radius Ship                 = 0.05
radius EnemyShip            = 0.05
radius Projectile           = 0.02
radius EnemyProjectile      = 0.02

overlap :: GameObject -> [GameObject] -> Bool
overlap o1 [] = False
overlap o1 (o2:os)
  | d < (r1 + r2) = True
  | (length os) > 0 = overlap o1 os
  | otherwise = False
  where
    r1 = radius $ gameObjectType o1
    r2 = radius $ gameObjectType o2
    d = norm $ location o1 ^-^ location o2

collide :: GameObject -> GameObject -> (Event CollisionCorrection, Event CollisionCorrection)
collide object other 
    | overlap object [other] = (Event objectCollisionCorrection , Event otherCollisionCorrection)
    | otherwise            = (NoEvent, NoEvent) where
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
        v1PostCollision = v2Colliding ^-^ v1Colliding
        --v1PostCollision = v2Colliding ^-^ v1
        v2PostCollision = v1Colliding ^-^ v2Colliding
        --v2PostCollision = v1Colliding ^-^ v2

        -- add the remaining velocities not involved in the collision
        deltaV1 = v1PostCollision ^+^ v1Remaining
        deltaV2 = v2PostCollision ^+^ v2Remaining

        -- calculate the location correction
        distance = norm difference
        radiusSum = radius (gameObjectType object) + radius (gameObjectType other)
        correction = radiusSum - distance
        deltaL1 =    (correction * 4) *^ collisionNormal
        deltaL2 = (-correction * 4) *^ collisionNormal

        objectCollisionCorrection = CollisionCorrection deltaL1 deltaV1 
        otherCollisionCorrection = CollisionCorrection deltaL2 deltaV2

torusfy :: Location -> Location
torusfy    (Vector x y)
    | x < -a = torusfy (Vector (x + 2 * a) y)
    | x >  a = torusfy (Vector (x - 2 * a) y)
    | y < -a = torusfy (Vector x (y + 2 * a))
    | y >  a = torusfy (Vector x (y - 2 * a))
    | otherwise = Vector x y
    where
     a = 1.05



-- Alternate Approach following the Yampa Arcade Paper: 

--  Game

--data ObjectInput = ObjectInput{
--    iHit :: Event (),
--    iUserInput :: UserInput,
--    iCollisionCorrection :: CollisionCorrection
--}

--data ObjectOutput = ObjectOutput{
--    gameObject :: GameObject,
--    killEvent :: Event (),
--    spawnEvent :: Event [GameObject]
--}

--type Object = SF ObjectInput ObjectOutput

--object :: GameObject ->               Object
--object (GameObject iLocation iVelocity iOrientation gameObjectType) = proc (ObjectInput hit userInput, collisionCorrection) -> do
--    orientation <- (iOrientation+) ^<< integral -< turn userInput
--    let acc = acceleration userInput *^ Vector (-sin orientation) (cos orientation)
--    velocity    <- (iVelocity ^+^) ^<< impulseIntegral -< (acc, deltaVelocity <$> collision_correction)
--    location    <- (iLocation ^+^) ^<< impulseIntegral -< (velocity, deltaLocation <$> collision_correction)
--    die         <- edge                                -< hit
--    returnA -<
--        -- depending on the object type, return different values
--        ObjectOutput {
--            gameObject = GameObject location velocity orientation gameObjectType,
--            killEvent = die,
--            spawnEvent =
--                fire ‘tag‘
--                [GameObject location (2 *^ velocity) orientation Projectile]
--        }

--gameCore :: IL Object -> SF (UserInput, IL ObjectOutput) (IL ObjectOutput)
--gameCore objs =
--    dpSwitch route
--    objs
--    (arr killOrSpawn >>> notYet)
--    (\sfs’ f -> gameCore (f sfs’))

--route :: (UserInput, IL ObjOutput) -> IL sf -> IL (ObjInput, sf)
--route    (userInput, outputs)         objectss     = mapIL routeAux objects where
--    routeAux (id, object) =
--        (ObjectInput {
--            iHit =  if id ‘elem‘ hits
--                    then Event ()
--                    else noEvent,
--            iUserInput = userInput,
--            iCollisionCorrection = ?
--        }, object)
--    hits = collisions (assocsIL (fmap gameObject outputs))


---- todo
--killOrSpawn :: (a, IL ObjectOutput) -> (Event (IL Object->IL Object))
--killOrSpawn    (_, outputs) = foldl (mergeBy (.)) noEvent es where
--    es :: [Event (IL Object -> IL Object)]
--    es = [ mergeBy (.)
--            (killEvent oo
--            ‘tag‘ (deleteIL k))
--            (fmap (foldl (.) id
--            . map insertIL_)
--            (spawnEvent oo))
--         | (k,oo) <- assocsIL outputs ]