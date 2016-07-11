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

animateTwoGameObjects :: GameObject -> GameObject -> SF GameInput (GameObject, GameObject)
animateTwoGameObjects gameObject otherObject = proc (gameInput) -> do
    rec 
        let (collisionCorrection1, collisionCorrection2) = collide gaOb otOb
        (gaOb, otOb) <- iPre (gameObject, otherObject) -< (object1, object2)
        let input = if gameObjectType gameObject == Ship then gameInput else  GameInput 0.0 0.0
        object1 <- animateGameObject gameObject   -< (collisionCorrection1, input)
        let otherInput = if gameObjectType otherObject == Ship then gameInput else  GameInput 0.0 0.0
        object2 <- animateGameObject otherObject  -< (collisionCorrection2, otherInput)
    returnA -< (object1, object2)

--animateGameObjectWithAllOthers:: GameObject -> [GameObject] -> SF GameInput (GameObject, [GameObject])
--animateGameObjectWithAllOthers iObject []               =  proc (gameInput) -> do
--    let input = if gameObjectType iObject == Ship then gameInput else  GameInput 0.0 0.0
--    object  <- animateGameObject iObject            -< (NoEvent, input)
--    returnA -< (object, [])
--animateGameObjectWithAllOthers iObject iOthers = proc (gameInput) -> do
--    ((object:objects), _) <- animateGameObjectWithAllOthersInner [iObject] iOthers -< gameInput
--    returnA               -< (object, objects)

--animateGameObjectWithAllOthersInner :: [GameObject] -> [GameObject] -> SF GameInput ([GameObject], [GameObject])
--animateGameObjectWithAllOthersInner (objects)          []               = proc (gameInput) -> do
--    returnA -< (objects, [])
--animateGameObjectWithAllOthersInner (iObject:iObjects) (iOther:iOthers) = proc (gameInput) -> do
--    (object, other)   <- animateTwoGameObjects iObject iOther                                       -< gameInput
--    (objects, others) <- animateGameObjectWithAllOthersInner ((ob:iObjects) ++ [ot]) iOthers -< (gameInput, object, other)
--    returnA         -< (objects, others)


--game :: GameLevel ->                      SF GameInput GameLevel
--game (GameLevel [])                    = proc (input) -> do
--    returnA         -< GameLevel []
--game (GameLevel (iObject:iTail))       = proc (input) -> do
--    (object, tail) <- animateGameObjectWithAllOthers iObject iTail -< input
--    -- same as above: was: game (GameLevel tail)
--    (GameLevel objects) <- game (GameLevel iTail) -< input
--    returnA         -< GameLevel $ [object] ++ objects

-- colliding works for the first and second, third and fourth object etc.
game :: GameLevel ->                      SF GameInput GameLevel
game    (GameLevel [])                    = proc (input) -> do
    returnA         -< GameLevel []
game    (GameLevel (iObject:[]))          = proc (input) -> do
    object          <- animateGameObject iObject            -< (NoEvent, input)
    returnA         -< GameLevel [object]
game (GameLevel (iObject:(iOther:iTail))) = proc (input) -> do
    (object, other) <- animateTwoGameObjects iObject iOther -< input
    tail            <- game (GameLevel iTail)               -< input
    returnA         -< GameLevel (object:(other:(objects tail)))

---- animates only two objects
---- iX stands for the initial value of X
--game :: (GameObject, GameObject) -> SF GameInput GameLevel
--game (iPlayer, iAsteroid)            = proc (gameInput) -> do
--    (player, asteroid) <- animateTwoGameObjects iPlayer iAsteroid -< gameInput
--    returnA            -< GameLevel [player, asteroid]


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

initialGameState :: GameLevel
initialGameState = GameLevel [initialAsteroid, otherAsteroid, thirdAsteroid, initialShip]

initialShip :: GameObject
initialShip = GameObject (Vector 0.0 0.0) (Vector 0.0 0.0) 0.0 Ship

initialEnemies :: [GameObject]
initialEnemies = []

initialAsteroid :: GameObject
initialAsteroid = GameObject (Vector 0.5 0.5) (Vector (-0.1) 0.0) 0.0 (Asteroid 1.0 shape) where
    shape = Shape [  (Vector   0.000    0.050 ),
                     (Vector   0.040    0.030 ),
                     (Vector   0.030    0.040 ),
                     (Vector   0.050    0.000 ),
                     (Vector   0.030  (-0.040)),
                     (Vector   0.040  (-0.030)),
                     (Vector   0.000  (-0.050)),
                     (Vector (-0.040) (-0.030)),
                     (Vector (-0.030) (-0.040)),
                     (Vector (-0.050)   0.000 ),
                     (Vector (-0.050)   0.000 ),
                     (Vector (-0.040)   0.030 )]

otherAsteroid :: GameObject
otherAsteroid = GameObject (Vector (-0.5) (-0.5)) (Vector 0.0 0.1) 0.0 (Asteroid 1.0 shape) where
    shape = Shape [  (Vector   0.000    0.050 ),
                     (Vector   0.040    0.030 ),
                     (Vector   0.030    0.040 ),
                     (Vector   0.050    0.000 ),
                     (Vector   0.030  (-0.040)),
                     (Vector   0.040  (-0.030)),
                     (Vector   0.000  (-0.050)),
                     (Vector (-0.040) (-0.030)),
                     (Vector (-0.030) (-0.040)),
                     (Vector (-0.050)   0.000 ),
                     (Vector (-0.050)   0.000 ),
                     (Vector (-0.040)   0.030 )]

thirdAsteroid :: GameObject
thirdAsteroid = GameObject (Vector 0.5 (-0.5)) (Vector 0.0 0.1) 0.0 (Asteroid 1.0 shape) where
    shape = Shape [  (Vector   0.000    0.050 ),
                     (Vector   0.040    0.030 ),
                     (Vector   0.030    0.040 ),
                     (Vector   0.050    0.000 ),
                     (Vector   0.030  (-0.040)),
                     (Vector   0.040  (-0.030)),
                     (Vector   0.000  (-0.050)),
                     (Vector (-0.040) (-0.030)),
                     (Vector (-0.030) (-0.040)),
                     (Vector (-0.050)   0.000 ),
                     (Vector (-0.050)   0.000 ),
                     (Vector (-0.040)   0.030 )]

