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
import Physics

-- iX stands for the initial value of X
animateGameObject :: GameObject ->                                             SF (Event CollisionCorrection, GameInput) GameObject
animateGameObject (GameObject iLocation iVelocity iOrientation gameObjectType) = proc (collisionCorrection, gameInput) -> do
    orientation <- (iOrientation+) ^<< integral -< turn gameInput
    let acc = acceleration gameInput *^ Vector (-sin orientation) (cos orientation)
    velocity    <- (iVelocity ^+^) ^<< impulseIntegral -< (acc, deltaVelocity <$> collisionCorrection)
    location    <- (iLocation ^+^) ^<< impulseIntegral -< (velocity, deltaLocation <$> collisionCorrection)
    returnA     -< GameObject location velocity orientation gameObjectType

animateTwoGameObjects :: GameObject -> GameObject -> SF (GameInput) (GameObject, GameObject)
animateTwoGameObjects gameObject otherObject = proc (gameInput) -> do
    rec 
        let (collisionCorrection1, collisionCorrection2) = collide gaOb otOb
        (gaOb, otOb) <- iPre (gameObject, otherObject) -< (object1, object2)
        object1 <- animateGameObject gameObject  -< (collisionCorrection1, gameInput)
        object2 <- animateGameObject otherObject  -< (collisionCorrection2, GameInput 0.0 0.0)
    returnA -< (object1, object2)

--animateGameObjects :: [GameObject] -> [SF GameInput GameObject]
--animateGameObjects = animateGameObjects


-- iX stands for the initial value of X
game :: (GameObject, GameObject) -> SF GameInput GameLevel
game (iPlayer, iAsteroid)            = proc (gameInput) -> do
    (player, asteroid) <- animateTwoGameObjects iPlayer iAsteroid -< gameInput
    returnA            -< GameLevel [player,asteroid]


-- Main

main :: IO ()
main    = do
    input <- newIORef (GameInput 0.0 0.0)
    output <- newIORef (EmptyLevel)
    t <- getCurrentTime
    time <- newIORef t
    window <- initGL
    handle <- reactInit (return (GameInput 0.0 0.0)) (actuator output) $ game initialGameState
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> handleInput window input $ Event $ KeyboardInput key keyState modifiers)
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

initialGameState :: (GameObject, GameObject)
initialGameState = (initialShip, initialAsteroid)

initialShip :: GameObject
initialShip = GameObject (Vector 0.0 0.0) (Vector 0.0 0.0) 0.0 Ship

initialEnemies :: [GameObject]
initialEnemies = []

initialAsteroid :: GameObject
initialAsteroid = GameObject (Vector 0.5 0.5) (Vector (-0.1) 0.0) 0.0 (Asteroid 1.0 shape)
  where
    shape = Shape [ (Vector 0.000  0.050),
                    (Vector 0.040  0.030),
                    (Vector 0.030  0.040),
                    (Vector 0.050  0.000),
                    (Vector 0.030 (-0.040)),
                    (Vector 0.040 (-0.030)),
                    (Vector 0.000 (-0.050)),
                    (Vector (-0.040) (-0.030)),
                    (Vector (-0.030) (-0.040)),
                    (Vector (-0.050)  0.000),
                    (Vector (-0.050)  0.000),
                    (Vector (-0.040)  0.030)]
