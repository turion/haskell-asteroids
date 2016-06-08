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

type Location = Vector
type Velocity = Vector


-- Ball

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
 
renderBall :: Location -> IO()
renderBall location = do
    clear[ColorBuffer]
    renderPrimitive Points $ do
        vertex $ (Vertex3 0 (realToFrac (y location))   0 :: Vertex3 GLfloat)
    swapBuffers

-- Graphics

idle :: IORef (UTCTime) -> ReactHandle () (IO ()) -> IO()
idle time handle = do
    now <- getCurrentTime
    before <- readIORef time
    let deltaTime = realToFrac $ diffUTCTime now before
    _ <- react handle (deltaTime, Nothing)
    writeIORef time now
    return ()


mainSF :: SF () (IO ())
mainSF = bouncingBall (Vector 0.0 1.0) >>^ \(location, velocity) -> renderBall location

initGL ::  IO ()
initGL = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow       "Bouncing Ball!"
    return ()

-- Main

main :: IO ()
main = do
    t <- getCurrentTime
    time <- newIORef t
    handle <- reactInit (initGL) (\_ _ b -> b >> return False) mainSF 
    displayCallback $= return()
    idleCallback $= Just (idle time handle)
    t' <- getCurrentTime
    writeIORef time t'
    mainLoop
