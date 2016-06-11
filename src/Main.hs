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

type Location = Vector
type Velocity = Vector
type Acceleration = Vector


-- Logic

infinitelyFallingBall :: Location -> Velocity -> SF() (Location, Velocity)
infinitelyFallingBall location velocity = proc () -> do
    v <- ((y velocity)+)^<< integral -< -9.81 -- gravity
    y <- ((y location)+)^<< integral -< v
    returnA -< (Vector 0.0 y, Vector 0.0 v)

fallingBall :: Location -> Velocity -> SF()((Location, Velocity), Event(Location, Velocity))
fallingBall location velocity = proc () -> do
    yv@(loc, _) <- infinitelyFallingBall location velocity -< ()
    hit       <- edge              -< y loc <= 0
    returnA -< (yv, hit `tag` yv)

bouncingBall :: Location -> SF()(Location, Velocity)
bouncingBall location = bbAux location (Vector 0.0 0.0)
    where bbAux location velocity = switch(fallingBall location velocity) $ \(y, (Vector vX vY)) -> bbAux y (Vector vX (-vY * 9 / 10))

movingBall :: Location ->  Velocity ->  Acceleration ->  SF()(Location, Velocity, Acceleration)
movingBall    loc0         vel0         acc0             = proc () -> do
    vX <- ((x vel0)+) ^<< integral -< x acc0
    vY <- ((y vel0)+) ^<< integral -< y acc0
    lX <- ((x loc0)+) ^<< integral -< vX
    lY <- ((y loc0)+) ^<< integral -< vY
    returnA -< (Vector lX lY, Vector vX vY, Vector (x acc0) (y acc0))

-- Graphics

idle :: IORef (Acceleration) -> IORef (Location) -> IORef (UTCTime) -> ReactHandle () (IO Location) -> IO()
idle    input                   output              time               handle                         = do
    acceleration <- readIORef input
    now <- getCurrentTime
    before <- readIORef time
    let deltaTime = realToFrac $ diffUTCTime now before
    _ <- react handle (deltaTime, Nothing)
    let programOutput = acceleration
    writeIORef output programOutput
    writeIORef time now
    postRedisplay Nothing    
    return ()

initGL ::  IO ()
initGL = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow       "Bouncing Ball!"
    return ()
 
renderBall :: Location -> IO()
renderBall location = do
    clear[ColorBuffer]
    renderPrimitive Points $ do
        vertex $ (Vertex3 (realToFrac (x location)) (realToFrac (y location)) 0 :: Vertex3 GLfloat)
    swapBuffers


-- UI

parseInput :: Event Input -> Acceleration
parseInput    (Event (Keyboard (SpecialKey KeyUp)    (Down) _))   =  (Vector   0.0   1.0 )
parseInput    (Event (Keyboard (SpecialKey KeyUp)    (Up)   _))   =  (Vector   0.0   0.0 )
parseInput    (Event (Keyboard (SpecialKey KeyDown)  (Down) _))   =  (Vector   0.0 (-1.0))
parseInput    (Event (Keyboard (SpecialKey KeyDown)  (Up)   _))   =  (Vector   0.0   0.0 )
parseInput    (Event (Keyboard (SpecialKey KeyRight) (Down) _))   =  (Vector   1.0   0.0 )
parseInput    (Event (Keyboard (SpecialKey KeyLeft)  (Down) _))   =  (Vector (-1.0)  0.0 )
parseInput    _                                                   =  (Vector   0.0   0.0 )


-- Main

main :: IO ()
main = do
    input <- newIORef (Vector 0.0 0.0)
    output <- newIORef (Vector 0.0 0.0)
    t <- getCurrentTime
    time <- newIORef t
    handle <- reactInit (initGL) (\_ _ b -> b >> return False) $ bouncingBall (Vector 0.0 1.0) >>^ \(location, velocity) -> return location
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> writeIORef input (parseInput $ Event $ Keyboard key keyState modifiers))
    displayCallback $= (readIORef output >>= renderBall)
    idleCallback $= Just (idle input output time handle)
    t' <- getCurrentTime
    writeIORef time t'
    mainLoop
