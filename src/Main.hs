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

-- iX stands for the initial value of X
animateGameObject :: GameObject ->                             SF (Event CollisionCorrection, UserInput, GameLevel) GameObject
animateGameObject iObject@(GameObject iLocation iVelocity iOrientation id gameObjectType) = proc (collisionCorrection, userInput, lastLevel) -> do
    let input = if gameObjectType == Ship then userInput else if gameObjectType == EnemyShip then aim id lastLevel else rotateAsteroid gameObjectType
    orientation      <- (iOrientation+) ^<< integral -< turn input
    let acc = acceleration input *^ Vector (-sin orientation) (cos orientation)
    velocity         <- (iVelocity ^+^) ^<< impulseIntegral -< (acc, deltaVelocity <$> collisionCorrection)
    preTorusLocation <- (iLocation ^+^) ^<< impulseIntegral -< (velocity, deltaLocation <$> collisionCorrection)
    let location = torusfy preTorusLocation
    returnA          -< GameObject location velocity orientation id gameObjectType

--lists are not necessarily of same size --> use length of list (maybe)
type CollisionEvents = [Event CollisionCorrection]

animateManyObjects :: GameLevel ->                   SF (CollisionEvents, UserInput, GameLevel) GameLevel
animateManyObjects    (GameLevel [])                 = arr $ const $ GameLevel []
animateManyObjects    (GameLevel (iObject:iObjects)) = proc ((event:events), input, lastLevel) -> do
    object              <- animateGameObject iObject       -< (event, input, lastLevel)
    (GameLevel objects) <- animateManyObjects (GameLevel (iObjects)) -< (events, input, lastLevel)
    returnA             -< GameLevel (object:objects)

collideAll :: GameLevel ->        CollisionEvents
collideAll    (GameLevel [])      = []
collideAll    (GameLevel objects) = collideWithRest objects (noEvents (GameLevel objects)) where
    collideWithRest :: [GameObject] ->  CollisionEvents -> CollisionEvents
    collideWithRest    []               collisionEvents    = collisionEvents
    collideWithRest    (object:objects) events             = parallelAdd [collideWithAllOthers object objects, restEvents] where
        restEvents = (NoEvent : collideWithRest objects (collideWithAllOthers object objects))

-- where for integers instead of Events, parallelAdd [[1,2,3,4,5,6], [6,5,4,3,2,1]] -> [7,7,7,7,7,7]
parallelAdd :: [CollisionEvents] -> CollisionEvents
parallelAdd                         = map sumEvents . transpose

collideWithAllOthers :: GameObject -> [GameObject] -> CollisionEvents
collideWithAllOthers    _             []              = [NoEvent]
collideWithAllOthers    object        others          = [sumEvents [fst (collide object other) | other <- others]] ++ [snd (collide object other) | other <- others]

sumEvents :: [Event CollisionCorrection] -> Event CollisionCorrection
sumEvents                                   = foldl (^+^) NoEvent

noEvents :: GameLevel -> CollisionEvents
noEvents = map (const NoEvent) . objects

game :: GameLevel -> SF UserInput GameLevel
game iLevel = proc (input) -> do
    rec
        -- iLevel <- ioLevel
        events <- iPre (noEvents iLevel)    -< collideAll level
        level  <- animateManyObjects iLevel -< (events, input, lastLevel)
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
    level <- generateLevel 5 10
    resetTriggered <- newIORef False
    handle <- reactInit (return (UserInput 0.0 0.0)) (actuator output) $ game level
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> handleInput window resetTriggered input $ Event $ KeyboardInput key keyState modifiers)
    idleCallback $= Just (idle input time handle)
    displayCallback $= (drawScreen output startTime)
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
