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

infinitelyFallingBall :: Location -> Velocity -> SF(Acceleration) (Location, Velocity)
infinitelyFallingBall    location    velocity    = proc (acceleration) -> do
    vX <- ((x velocity)+)^<< integral -< (x acceleration)
    vY <- ((y velocity)+)^<< integral -< -9.81 + (20 * y acceleration) -- gravity + input
    x  <- ((x location)+)^<< integral -< vX
    y  <- ((y location)+)^<< integral -< vY
    returnA -< (Vector x y, Vector vX vY)

fallingBall :: Location -> Velocity -> SF(Acceleration)((Location, Velocity), Event(Location, Velocity))
fallingBall    location    velocity    = proc (acceleration) -> do
    yv@(loc, _) <- infinitelyFallingBall location velocity -< (acceleration)
    hit         <- edge                                    -< y loc <= 0
    returnA     -< (yv, hit `tag` yv)

bouncingBall :: Location -> SF (Acceleration) (Location, Velocity)
bouncingBall    location    = bbAux location (Vector 0.0 0.0)
    where bbAux location velocity = switch(fallingBall location velocity) $ \(y, (Vector vX vY)) -> bbAux y (Vector vX (-vY * 9 / 10))

movingBall :: Location ->  Velocity ->  Acceleration ->  SF()(Location, Velocity, Acceleration)
movingBall    loc0         vel0         acc0             = proc () -> do
    vX <- ((x vel0)+) ^<< integral -< x acc0
    vY <- ((y vel0)+) ^<< integral -< y acc0
    lX <- ((x loc0)+) ^<< integral -< vX
    lY <- ((y loc0)+) ^<< integral -< vY
    returnA -< (Vector lX lY, Vector vX vY, Vector (x acc0) (y acc0))


-- Graphics

idle :: IORef (Acceleration) -> IORef (Location) -> IORef (UTCTime) -> ReactHandle Acceleration (Location, Velocity) -> IO()
idle    input                   output              time               handle                                           = do
    acceleration <- readIORef input
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
    
 
renderBall :: Location -> IO()
renderBall    location    = do
    clear[ColorBuffer]
    renderPrimitive Polygon $ do
        vertex $ (Vertex3 (realToFrac (x location))           (realToFrac (0.1 + y location))     0 :: Vertex3 GLfloat)
        vertex $ (Vertex3 (realToFrac (0.05 + x location))    (realToFrac ((-0.05) + y location)) 0 :: Vertex3 GLfloat)
        vertex $ (Vertex3 (realToFrac (x location))           (realToFrac (y location))           0 :: Vertex3 GLfloat)
        vertex $ (Vertex3 (realToFrac ((-0.05) + x location)) (realToFrac ((-0.05) + y location)) 0 :: Vertex3 GLfloat)
    swapBuffers


-- Main

main :: IO ()
main    = do
    input <- newIORef (Vector 0.0 0.0)
    output <- newIORef (Vector 0.0 0.0)
    t <- getCurrentTime
    time <- newIORef t
    initGL
    handle <- reactInit (return (Vector 0.0 0.0)) (actuator output) $ bouncingBall (Vector 0.0 1.0)
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> writeIORef input (parseInput $ Event $ Keyboard key keyState modifiers))
    idleCallback $= Just (idle input output time handle)
    displayCallback $= (readIORef output >>= renderBall)
    t' <- getCurrentTime
    writeIORef time t'
    mainLoop

actuator :: IORef Vector -> ReactHandle Vector (Location, Velocity) -> Bool -> (Location, Velocity) -> IO Bool
actuator    output          _                                          _       (location, velocity)    = do
    writeIORef output location
    return False
