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


-- Logic

movingShip :: Location -> Velocity -> Orientation -> SF GameInput (Location, Velocity, Orientation)
movingShip    location    velocity    orientation    = proc (GameInput acceleration deltaOrientation) -> do
    dO <- (orientation+) ^<< integral -< deltaOrientation
    vX <- ((x velocity)+)^<< integral -< (-sin dO) * acceleration
    vY <- ((y velocity)+)^<< integral -< cos dO * acceleration
    x  <- ((x location)+)^<< integral -< vX
    y  <- ((y location)+)^<< integral -< vY
    returnA -< (Vector x y, Vector vX vY, dO)

createShip :: Location -> SF GameInput (Location, Velocity, Orientation)
createShip    location    = movingShip location (Vector 0.0 0.0) 0.0


-- Graphics

idle :: IORef (GameInput) -> IORef (UTCTime) -> ReactHandle GameInput (Location, Velocity, Orientation) -> IO()
idle    userInput            time               handle                                                     = do
    input <- readIORef userInput
    now <- getCurrentTime
    before <- readIORef time
    let deltaTime = realToFrac $ diffUTCTime now before
    _ <- react handle (deltaTime, Just input)
    writeIORef time now
    postRedisplay Nothing    
    return ()

initGL ::  IO ()
initGL     = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow       "Haskelloids!"
    return ()
    
 
render :: (Location, Orientation) -> IO()
render    (location, orientation)    = do
    clear[ColorBuffer]
    preservingMatrix $ do
        translate $ (Vector3 (realToFrac (x location):: GLfloat) (realToFrac (y location):: GLfloat) 0)
        rotate (realToFrac orientation * 360 / (2 * pi) :: GLfloat) $ Vector3 0 0 1
        renderPrimitive Polygon $ do
            vertex $ (Vertex3   0.00    0.05  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.02  (-0.02) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.00    0.00  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.02) (-0.02) 0 :: Vertex3 GLfloat)
    swapBuffers


-- Main

main :: IO ()
main    = do
    input <- newIORef (GameInput 0.0 0.0)
    output <- newIORef (Vector 0.0 0.0, 0.0)
    t <- getCurrentTime
    time <- newIORef t
    initGL
    handle <- reactInit (return (GameInput 0.0 0.0)) (actuator output) $ createShip (Vector 0.0 0.0)
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> handleInput input $ Event $ Keyboard key keyState modifiers)
    idleCallback $= Just (idle input time handle)
    displayCallback $= (readIORef output >>= render)
    t' <- getCurrentTime
    writeIORef time t'
    mainLoop

actuator :: IORef (Location, Orientation) -> ReactHandle (GameInput) (Location, Velocity, Orientation) -> Bool -> (Location, Velocity, Orientation) -> IO Bool
actuator    output                           _                                                            _       (location, velocity, orientation)    = do
    writeIORef output (location, orientation)
    return False
