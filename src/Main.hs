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

movingShip :: Location -> Velocity -> Orientation -> SF(Acceleration, Orientation) (Location, Velocity, Orientation)
movingShip    location    velocity    orientation    = proc (acceleration, deltaOrientation) -> do
    dO <- (orientation+) ^<< integral -< deltaOrientation
    vX <- ((x velocity)+)^<< integral -< (-sin dO) * acceleration -- change to take orientation into consideration
    vY <- ((y velocity)+)^<< integral -< cos dO * acceleration -- change to take orientation into consideration
    x  <- ((x location)+)^<< integral -< vX
    y  <- ((y location)+)^<< integral -< vY
    returnA -< (Vector x y, Vector vX vY, dO)

createShip :: Location -> SF (Acceleration, Orientation) (Location, Velocity, Orientation)
createShip    location    = movingShip location (Vector 0.0 0.0) 0.0


-- Graphics

idle :: IORef (Acceleration, Orientation) -> IORef (UTCTime) -> ReactHandle (Acceleration, Orientation) (Location, Velocity, Orientation) -> IO()
idle    userInput                            time               handle                                                                       = do
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
    createWindow       "Bouncing Ball!"
    return ()
    
 
render :: (Location, Orientation) -> IO()
render    (location, orientation)    = do
    clear[ColorBuffer]
    preservingMatrix $ do
        translate $ (Vector3 (realToFrac (x location):: GLfloat) (realToFrac (y location):: GLfloat) 0)
        rotate (realToFrac orientation * 360 / (2 * pi) :: GLfloat) $ Vector3 0 0 1
        renderPrimitive Polygon $ do
            vertex $ (Vertex3   0.00    0.10  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.05  (-0.05) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.00    0.00  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.05) (-0.05) 0 :: Vertex3 GLfloat)
    swapBuffers


-- Main

main :: IO ()
main    = do
    input <- newIORef (0.0, 0.0)
    output <- newIORef (Vector 0.0 0.0, 0.0)
    t <- getCurrentTime
    time <- newIORef t
    initGL
    handle <- reactInit (return (0.0, 0.0)) (actuator output) $ createShip (Vector 0.0 0.0)
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> writeIORef input (parseInput $ Event $ Keyboard key keyState modifiers))
    idleCallback $= Just (idle input time handle)
    --(outputLoc, outputOr) <- (readIORef output, readIORef outputOrientation)
    displayCallback $= (readIORef output >>= render)
    t' <- getCurrentTime
    writeIORef time t'
    mainLoop

actuator :: IORef (Location, Double) -> ReactHandle (Acceleration, Orientation) (Location, Velocity, Orientation) -> Bool -> (Location, Velocity, Orientation) -> IO Bool
actuator    output          _                                                                 _       (location, velocity, orientation)    = do
    writeIORef output (location, orientation)
    return False
