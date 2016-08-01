{-# LANGUAGE Arrows #-}

import Control.Monad
import Control.Arrow
import Data.IORef
import Data.List
import Data.Time.Clock
import FRP.Yampa
-- Graphics
import FRP.Yampa.Simulation
import Graphics.UI.GLUT
import Control.Concurrent
-- Haskelloids
import Datatypes
import UI
import AI
import Graphics
import Physics
import Generator
import System.Random

-- iX stands for the initial value of X
animateGameObject :: GameObject ->                             SF (Event CollisionCorrection, UserInput, GameLevel) GameObject
animateGameObject (GameObject iLocation iVelocity iOrientation id Ship) = proc (collisionCorrection, userInput, lastLevel) -> do
    let input = userInput
    orientation      <- (iOrientation+) ^<< integral -< turn input
    let acc = acceleration input *^ Vector (-sin orientation) (cos orientation)
    velocity         <- (iVelocity ^+^) ^<< impulseIntegral -< (acc, deltaVelocity <$> collisionCorrection)
    preTorusLocation <- (iLocation ^+^) ^<< impulseIntegral -< (velocity, deltaLocation <$> collisionCorrection)
    let location = torusfy preTorusLocation
    returnA          -< GameObject location velocity orientation id Ship
animateGameObject (GameObject iLocation iVelocity iOrientation id gameObjectType) = proc (collisionCorrection, userInput, lastLevel) -> do
    let input = if gameObjectType == EnemyShip then aim id lastLevel else rotateAsteroid gameObjectType
    orientation      <- (iOrientation+) ^<< integral -< turn input
    let acc = acceleration input *^ Vector (-sin orientation) (cos orientation)
    velocity         <- (iVelocity ^+^) ^<< impulseIntegral -< (acc, deltaVelocity <$> collisionCorrection)
    preTorusLocation <- (iLocation ^+^) ^<< impulseIntegral -< (velocity, deltaLocation <$> collisionCorrection)
    let location = torusfy preTorusLocation
    returnA          -< GameObject location velocity orientation id gameObjectType

--lists are not necessarily of same size --> use length of list (maybe)
type CorrectionEvents = [Event CollisionCorrection]
type ExplosionEvents = [Event Circle]

animateManyObjects :: GameLevel ->                   SF (CorrectionEvents, UserInput, GameLevel) GameLevel
animateManyObjects    (GameLevel [])                 = arr $ const $ GameLevel []
animateManyObjects    (GameLevel (iObject:iObjects)) = proc ((event:events), input, lastLevel) -> do
    object              <- animateGameObject iObject       -< (event, input, lastLevel)
    (GameLevel objects) <- animateManyObjects (GameLevel (iObjects)) -< (events, input, lastLevel)
    returnA             -< GameLevel (object:objects)

collideAll :: GameLevel ->        (CorrectionEvents, ExplosionEvents)
collideAll    (GameLevel [])      = ([], [])
collideAll    (GameLevel objects) = collideWithRest objects emptyEvents where
    emptyEvents = ((noEvents (GameLevel objects)), [])
    collideWithRest :: [GameObject] ->  (CorrectionEvents, ExplosionEvents) -> (CorrectionEvents, ExplosionEvents)
    collideWithRest    []               events                                = events
    collideWithRest    (object:objects) (collisions, explosions)              = (correctionEvents, explosionEvents) where
        (objectCorrections, objectExplosions) = collideWithAllOthers object objects
        explosionEvents = objectExplosions ++ restExplosions
        correctionEvents = parallelAdd [objectCorrections, (NoEvent : restCorrections)]
        (restCorrections, restExplosions) = collideWithRest objects (objectCorrections, objectExplosions)

-- where for integers instead of Events, parallelAdd [[1,2,3,4,5,6], [6,5,4,3,2,1]] -> [7,7,7,7,7,7]
parallelAdd :: [CorrectionEvents] -> CorrectionEvents
parallelAdd                         = map sumEvents . transpose

collideWithAllOthers :: GameObject -> [GameObject] -> (CorrectionEvents, ExplosionEvents)
collideWithAllOthers    _             []              = ([NoEvent], [NoEvent])
collideWithAllOthers    object        others          = (corrections, explosions) where
    collisions = [collide object other | other <- others]
    corrections = [sumEvents firstCorrections] ++ otherCorrections
    (firstCorrections, otherCorrections) = getCorrections collisions
    explosions = getExplosions collisions

getExplosions :: [Event CollisionResult] ->                        ExplosionEvents
getExplosions    []                                                = []
getExplosions    (Event (Explosion (Circle center size)): results) = (Explosion (Circle center size): getExplosions results)
getExplosions    (_ : results)                                     = (NoEvent : getExplosions results)

getCorrections :: [Event CollisionResult] ->                  (CorrectionEvents, CorrectionEvents)
getCorrections    []                                          = ([], [])
getCorrections    (Event (Correction (left, right)): results) = ((Event left : objectCorrections), (Event right, otherCorrections)) where
    objectCorrections = fst (getCorrections results)
    otherCorrections = snd (getCorrections results)
getCorrections    (_                             : results)   = ((NoEvent: objectCorrections), (NoEvent: otherCorrections)) where
    objectCorrections = fst (getCorrections results)
    otherCorrections = snd (getCorrections results)

sumEvents :: [Event CollisionCorrection] -> Event CollisionCorrection
sumEvents                                    = foldl (^+^) NoEvent

noEvents :: GameLevel -> CorrectionEvents
noEvents = map (const NoEvent) . objects

game :: GameLevel -> SF UserInput GameLevel
game iLevel = proc (input) -> do
    rec
        -- iLevel <- ioLevel
        (correctionEvents, explosionEvents) <- iPre (noEvents iLevel)    -< collideAll level
        level  <- animateManyObjects iLevel -< (correctionEvents, input, lastLevel)
        lastLevel <- iPre (iLevel) -< level
    returnA -< level

-- Main

main :: IO ()
main    = do
    window <- initGL
    input <- newIORef (UserInput 0.0 0.0)
    output <- newIORef (EmptyLevel)
    fullScreen
    reshapeCallback $= Just reshape
    showText "Haskelloids" (Vector (-0.55) 0) [0.5, 0.0, 0.5] 0.002 Title
    threadDelay 2000000
    t <- getCurrentTime
    time <- newIORef t
    startTime <- newIORef t
    randomGenerator <- getStdGen
    let level = generateLevel 5 10 randomGenerator
    gameState <- newIORef $ GameState 1 3 0 1000 False
    resetTriggered <- newIORef False
    handle <- reactInit (return (UserInput 0.0 0.0)) (actuator output) $ game level
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> handleInput window gameState resetTriggered input $ Event $ KeyboardInput key keyState modifiers)
    idleCallback $= Just (idle input time handle)
    displayCallback $= (drawScreen gameState output startTime)
    mainLoop

idle :: IORef UserInput -> IORef UTCTime -> ReactHandle UserInput GameLevel -> IO()
idle    userInput          time             handle                             = do
    input <- readIORef userInput
    now <- getCurrentTime
    before <- readIORef time
    let deltaTime = realToFrac $ diffUTCTime now before
    _ <- react handle (deltaTime, Just input)
    writeIORef time now
    postRedisplay Nothing

actuator :: IORef GameLevel -> ReactHandle UserInput GameLevel -> Bool -> GameLevel -> IO Bool
actuator    output             _                                  _       gameLevel    = do
    writeIORef output gameLevel
    return False
