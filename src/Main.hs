{-# LANGUAGE Arrows #-}

import Control.Monad
import Control.Arrow
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
import FRP.Yampa.Simulation
import Graphics.UI.GLUT
import Control.Concurrent

type Pos = Double
type Vel = Double

fallingBall :: Pos -> Vel -> SF() (Pos, Vel)
fallingBall y0 v0 = proc () -> do
    v <- (v0+)^<< integral -< -9.81 -- gravity
    y <- (y0+)^<< integral -< v
    returnA -< (y,v)

fallingBall' :: Pos -> Vel -> SF()((Pos, Vel), Event(Pos, Vel))
fallingBall' y0 v0 = proc () -> do
    yv@(y, _) <- fallingBall y0 v0 -< ()
    hit       <- edge              -< y <= 0
    returnA -< (yv, hit `tag` yv)

bouncingBall :: Pos -> SF()(Pos, Vel)
bouncingBall y0 = bbAux y0 0.0
    where bbAux y0 v0 = switch(fallingBall' y0 v0) $ \(y, v) -> bbAux (-y) (-v * 9 / 10)

renderBall :: Pos -> IO()
renderBall pos = do
    clear[ColorBuffer]
    renderPrimitive Points $ do
        vertex $ (Vertex3 0 (realToFrac pos)   0 :: Vertex3 GLfloat)
    swapBuffers

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

idle :: IORef (UTCTime) -> ReactHandle () (IO ()) -> IO()
idle time handle = do
    now <- getCurrentTime
    before <- readIORef time
    let deltaTime = realToFrac $ diffUTCTime now before
    _ <- react handle (deltaTime, Nothing)
    writeIORef time now
    return ()


mainSF :: SF () (IO ())
mainSF = bouncingBall 1.0 >>^ \(pos, vel) -> renderBall pos

initGL ::  IO ()
initGL = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow       "Bouncing Ball!"
    return ()
