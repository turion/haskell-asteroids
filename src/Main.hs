{-# LANGUAGE Arrows #-}

import Control.Monad
import Control.Arrow
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
-- Graphics
import FRP.Yampa.Simulation
import Graphics.UI.GLUT
import Control.Concurrent
-- Haskelloids
import Datatypes
import UI
import Graphics

-- iX stands for the initial value of X
animateGameObject :: GameObject ->                                             SF GameInput GameObject
animateGameObject (GameObject iLocation iVelocity iOrientation gameObjectType) = proc (GameInput acceleration deltaOrientation) -> do
    orientation <- (iOrientation+) ^<< integral -< deltaOrientation
    velocity    <- (iVelocity ^+^) ^<< integral -< acceleration *^ Vector (-sin orientation) (cos orientation)
    location    <- (iLocation ^+^) ^<< integral -< velocity
    returnA     -< GameObject location velocity orientation gameObjectType


animateGameObjects :: [GameObject] -> SF GameInput [GameObject]
animateGameObjects    []              = proc (input) -> do
    returnA    -< []
animateGameObjects (iGameObject:tail) = proc (input) -> do
    gameObject <- animateGameObject iGameObject -< input
    animateGameObjects tail                     -< input
    returnA    -< (gameObject:tail)


-- iX stands for the initial value of X
game :: GameLevel ->                                                        SF GameInput GameLevel
game (GameLevel iPlayer iEnemies iAsteroids iProjectiles iEnemyProjectiles) = proc (gameInput) -> do
    player           <- animateGameObject iPlayer            -< gameInput
    enemies          <- animateGameObjects iEnemies          -< GameInput 0.0 0.0
    asteroids        <- animateGameObjects iAsteroids        -< GameInput 0.0 0.0
    projectiles      <- animateGameObjects iProjectiles      -< GameInput 0.0 0.0
    enemyProjectiles <- animateGameObjects iEnemyProjectiles -< GameInput 0.0 0.0
    returnA          -< GameLevel player enemies asteroids iProjectiles iEnemyProjectiles


-- Main

main :: IO ()
main    = do
    input <- newIORef (GameInput 0.0 0.0)
    output <- newIORef (EmptyLevel)
    t <- getCurrentTime
    time <- newIORef t
    window <- initGL
    handle <- reactInit (return (GameInput 0.0 0.0)) (actuator output) $ game initialGameLevel
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> handleInput window input $ Event $ Keyboard key keyState modifiers)
    idleCallback $= Just (idle input time handle)
    displayCallback $= (readIORef output >>= renderLevel)
    t' <- getCurrentTime
    writeIORef time t'
    reshapeCallback $= Just reshape
    fullScreen
    mainLoop


idle :: IORef GameInput -> IORef UTCTime -> ReactHandle GameInput GameLevel -> IO()
idle    gameInput          time             handle                             = do
    input <- readIORef gameInput
    now <- getCurrentTime
    before <- readIORef time
    let deltaTime = realToFrac $ diffUTCTime now before
    _ <- react handle (deltaTime, Just input)
    writeIORef time now
    postRedisplay Nothing    


actuator :: IORef GameLevel -> ReactHandle GameInput GameLevel -> Bool -> GameLevel -> IO Bool
actuator    output             _                                  _       gameLevel    = do
    writeIORef output gameLevel
    return False

initialGameLevel :: GameLevel
initialGameLevel = (GameLevel initialShip initialEnemies initialAsteroids [] [])

initialShip :: GameObject
initialShip = GameObject (Vector 0.0 0.0) (Vector 0.0 0.0) 0.0 Ship

initialEnemies :: [GameObject]
initialEnemies = []

initialAsteroids :: [GameObject]
initialAsteroids = [(GameObject (Vector 0.5 0.5) (Vector (-0.1) 0.0) 0.0 (Asteroid 1.0))]
