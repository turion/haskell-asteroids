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

infinitelyFallingBall :: Location -> Velocity -> Orientation -> SF(Acceleration) (Location, Velocity, Orientation)
infinitelyFallingBall    location    velocity    orientation    = proc (acceleration) -> do
    vX <- ((x velocity)+)^<< integral -< (x acceleration)
    vY <- ((y velocity)+)^<< integral -< -9.81 + (20 * y acceleration) -- gravity + input
    x  <- ((x location)+)^<< integral -< vX
    y  <- ((y location)+)^<< integral -< vY
    returnA -< (Vector x y, Vector vX vY, 0.0)

fallingBall :: Location -> Velocity -> Orientation -> SF(Acceleration)((Location, Velocity, Orientation), Event(Location, Velocity, Orientation))
fallingBall    location    velocity    orientation    = proc (acceleration) -> do
    object@(loc, _, _) <- infinitelyFallingBall location velocity orientation -< (acceleration)
    hit         <- edge                                                       -< y loc <= 0
    returnA     -< (object, hit `tag` object)

bouncingBall :: Location -> SF (Acceleration) (Location, Velocity, Orientation)
bouncingBall    location    = bbAux location (Vector 0.0 0.0) 0.0
    where bbAux location velocity orientation = switch(fallingBall location velocity orientation) $ \(loc, (Vector vX vY), ori) -> bbAux loc (Vector vX (-vY * 9 / 10)) ori

movingBall :: Location ->  Velocity ->  Acceleration ->  SF()(Location, Velocity, Acceleration)
movingBall    loc0         vel0         acc0             = proc () -> do
    vX <- ((x vel0)+) ^<< integral -< x acc0
    vY <- ((y vel0)+) ^<< integral -< y acc0
    lX <- ((x loc0)+) ^<< integral -< vX
    lY <- ((y loc0)+) ^<< integral -< vY
    returnA -< (Vector lX lY, Vector vX vY, Vector (x acc0) (y acc0))


-- Graphics

idle :: IORef (Acceleration, Orientation) -> IORef (UTCTime) -> ReactHandle Acceleration (Location, Velocity, Orientation) -> IO()
idle    input                                time               handle                                                        = do
    (acceleration, orientation) <- readIORef input
    now <- getCurrentTime
    before <- readIORef time
    let deltaTime = realToFrac $ diffUTCTime now before
    _ <- react handle (deltaTime, Just acceleration)
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
        rotate (realToFrac orientation :: GLfloat) $ Vector3 0 0 1
        translate $ (Vector3 (realToFrac (x location):: GLfloat) (realToFrac (y location):: GLfloat) 0)
        renderPrimitive Polygon $ do
            vertex $ (Vertex3   0.00    0.10  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.05  (-0.05) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.00    0.00  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.05) (-0.05) 0 :: Vertex3 GLfloat)
    swapBuffers


-- Main

main :: IO ()
main    = do
    input <- newIORef (Vector 0.0 0.0, 0.0)
    output <- newIORef (Vector 0.0 0.0, 0.0)
    t <- getCurrentTime
    time <- newIORef t
    initGL
    handle <- reactInit (return (Vector 0.0 0.0)) (actuator output) $ bouncingBall (Vector 0.0 1.0)
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> writeIORef input (parseInput $ Event $ Keyboard key keyState modifiers))
    idleCallback $= Just (idle input time handle)
    --(outputLoc, outputOr) <- (readIORef output, readIORef outputOrientation)
    displayCallback $= (readIORef output >>= render)
    t' <- getCurrentTime
    writeIORef time t'
    mainLoop

actuator :: IORef (Vector, Double) -> ReactHandle Vector (Location, Velocity, Orientation) -> Bool -> (Location, Velocity, Orientation) -> IO Bool
actuator    output          _                                                                 _       (location, velocity, orientation)    = do
    writeIORef output (location, orientation)
    return False
