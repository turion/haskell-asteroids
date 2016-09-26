{-# LANGUAGE Arrows #-}
module Game (
    game
) where

import Control.Monad
import Control.Arrow
import Data.List
import FRP.Yampa
-- Haskelloids
import Datatypes
import UI
import AI
import Physics
import Tuple


-- iX stands for the initial value of X
game :: GameLevel -> SF UserInput GameLevel
game    iLevel       = gameLoop iLevel `switch` (game . uncurry3 applyEvents)

gameLoop :: GameLevel -> SF UserInput (GameLevel, Event (GameLevel, ExplosionEvents, FireEvent))
gameLoop iLevel = proc (input) -> do
    rec
        (correctionEvents, explosionEvents) <- iPre ((noEvents iLevel), []) -< collideAll level
        level  <- animateManyObjects iLevel -< (correctionEvents, input, lastLevel)
        lastLevel <- iPre (iLevel) -< level
        let fireEvent = if fire input then Event (Fire True) else NoEvent
        let event = if length explosionEvents > 0 || fireEvent /= NoEvent then Event (level, explosionEvents, fireEvent) else NoEvent
    returnA -< (level, event)

type CorrectionEvents = [Event CollisionCorrection]
type ExplosionEvents = [Event Explosion]
type FireEvent = Event Fire

applyEvents :: GameLevel -> ExplosionEvents ->           FireEvent -> GameLevel
applyEvents    level        []                           NoEvent    = level
applyEvents    level        []                           _          = applyFire level
applyEvents    level        (NoEvent:explosions)         fire       = applyEvents level explosions fire
applyEvents    level        (Event explosion:explosions) fire       = applyEvents (applyExplosion level explosion) explosions fire

applyExplosion :: GameLevel ->                 Explosion -> GameLevel
applyExplosion    level                        explosion    = applyExplosion' level (GameLevel []) explosion where
    applyExplosion' :: GameLevel ->                 GameLevel ->       Explosion ->                          GameLevel
    applyExplosion'    (GameLevel [])               result             explosion                             = result
    applyExplosion'    (GameLevel (object:objects)) (GameLevel result) (Explosion explosionCenter explosionRadius) = if distance > radiiSum
        then applyExplosion' (GameLevel objects) (GameLevel (object:result)) explosion
        else applyExplosion' (GameLevel objects) (GameLevel result) explosion where
        distance = norm $ (location object) ^-^ explosionCenter
        radiiSum = (radius (gameObjectType object)) + explosionRadius

applyFire :: GameLevel ->               GameLevel
applyFire (GameLevel (object: objects)) = if gameObjectType object == Ship then GameLevel (ship:(projectile:objects)) else GameLevel (object:objects) where
    ship = object
    projectile = GameObject pLocation pVelocity pOrientation pId Projectile
    pLocation = getProjectileLocation ship
    pVelocity = Vector (cos(pOrientation) * 0.1) (sin(pOrientation) * 0.1)
    pOrientation = orientation ship
    pId = -1

collideAll :: GameLevel ->        (CorrectionEvents, ExplosionEvents)
collideAll    (GameLevel [])      = ([], [])
collideAll    (GameLevel objects) = collideWithRest objects emptyEvents where
    emptyEvents = ((noEvents (GameLevel objects)), [])
    collideWithRest :: [GameObject] ->  (CorrectionEvents, ExplosionEvents) -> (CorrectionEvents, ExplosionEvents)
    collideWithRest    []               events                                = events
    collideWithRest    (object:objects) (collisions, explosions)              = (correctionEvents, explosionEvents) where
        (objectCorrections, objectExplosions) = collideWithAllOthers object objects
        explosionEvents                       = objectExplosions ++ restExplosions
        correctionEvents                      = parallelAdd [objectCorrections, (NoEvent : restCorrections)]
        (restCorrections, restExplosions)     = collideWithRest objects (objectCorrections, objectExplosions)

collideWithAllOthers :: GameObject -> [GameObject] -> (CorrectionEvents, ExplosionEvents)
collideWithAllOthers    _             []              = ([NoEvent], [NoEvent])
collideWithAllOthers    object        others          = (corrections, explosions) where
    collisions = [collide object other | other <- others]
    corrections = [sumEvents firstCorrections] ++ otherCorrections
    (firstCorrections, otherCorrections) = getCorrections collisions
    explosions = getExplosions collisions

getExplosions :: [Event CollisionResult] ->                        ExplosionEvents
getExplosions    []                                                = []
getExplosions    (Event (Reduction (Explosion center size)): results) = (Event (Explosion center size): getExplosions results)
getExplosions    (_: results)                                      = (NoEvent: getExplosions results)

getCorrections :: [Event CollisionResult] ->                  (CorrectionEvents, CorrectionEvents)
getCorrections    []                                          = ([], [])
getCorrections    (Event (Correction (left, right)): results) = ((Event left: objectCorrections), (Event right: otherCorrections)) where
    objectCorrections = fst (getCorrections results)
    otherCorrections = snd (getCorrections results)
getCorrections    (_: results)                                = ((NoEvent: objectCorrections), (NoEvent: otherCorrections)) where
    objectCorrections = fst (getCorrections results)
    otherCorrections = snd (getCorrections results)

animateManyObjects :: GameLevel ->                   SF (CorrectionEvents, UserInput, GameLevel) GameLevel
animateManyObjects    (GameLevel [])                 = arr $ const $ GameLevel []
animateManyObjects    (GameLevel (iObject:iObjects)) = proc ((event:events), input, lastLevel) -> do
    object              <- animateGameObject iObject                 -< (event, input, lastLevel)
    (GameLevel objects) <- animateManyObjects (GameLevel (iObjects)) -< (events, input, lastLevel)
    returnA             -< GameLevel (object:objects)

animateGameObject :: GameObject ->                             SF (Event CollisionCorrection, UserInput, GameLevel) GameObject
animateGameObject (GameObject iLocation iVelocity iOrientation id Ship) = proc (collisionCorrection, userInput, lastLevel) -> do
    let input = userInput
    orientation      <- (iOrientation+) ^<< integral -< turn input
    let acc = acceleration input *^ Vector (-sin orientation) (cos orientation)
    velocity         <- (iVelocity ^+^) ^<< impulseIntegral -< (acc, deltaVelocity <$> collisionCorrection)
    preTorusLocation <- (iLocation ^+^) ^<< impulseIntegral -< (velocity, deltaLocation <$> collisionCorrection)
    let location = torusfy preTorusLocation
    returnA          -< GameObject location velocity orientation id Ship
animateGameObject (GameObject iLocation iVelocity iOrientation id iGameObjectType) = proc (collisionCorrection, userInput, lastLevel) -> do
    let input = handleType iGameObjectType lastLevel
    orientation      <- (iOrientation+) ^<< integral -< turn input
    let acc = acceleration input *^ Vector (-sin orientation) (cos orientation)
    velocity         <- (iVelocity ^+^) ^<< impulseIntegral -< (acc, deltaVelocity <$> collisionCorrection)
    preTorusLocation <- (iLocation ^+^) ^<< impulseIntegral -< (velocity, deltaLocation <$> collisionCorrection)
    let location = torusfy preTorusLocation
    returnA          -< GameObject location velocity orientation id iGameObjectType where
        handleType objectType level = case objectType of
            EnemyShip      -> aim id level
            Asteroid _ _ _ -> rotateAsteroid objectType
            _              -> UserInput 0.0 0.0 False

-- where for integers instead of Events, parallelAdd [[1,2,3,4,5,6], [6,5,4,3,2,1]] -> [7,7,7,7,7,7]
parallelAdd :: [CorrectionEvents] -> CorrectionEvents
parallelAdd                         = map sumEvents . transpose

sumEvents :: [Event CollisionCorrection] -> Event CollisionCorrection
sumEvents                                    = foldl (^+^) NoEvent

noEvents :: GameLevel -> CorrectionEvents
noEvents = map (const NoEvent) . objects
