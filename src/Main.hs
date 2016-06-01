{-# LANGUAGE Arrows #-}

import Control.Monad
import Control.Arrow
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
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
    flush
    threadDelay 500

main :: IO ()
main = do
    t <- getCurrentTime
    timeRef <- newIORef t
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Bouncing Ball"
    let init        = putStrLn "Bouncing Ball:"
        actuate _ (pos, vel) = renderBall pos >> return False
        sense   _   = do
            now      <- getCurrentTime
            lastTime <- readIORef timeRef
            writeIORef timeRef now
            let dt = now `diffUTCTime` lastTime
            return (realToFrac dt, Just ())
    reactimate init sense actuate $ bouncingBall 1