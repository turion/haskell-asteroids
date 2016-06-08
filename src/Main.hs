{-# LANGUAGE Arrows #-}

import Control.Monad
import Control.Arrow
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
import Datatypes

type Location = Vector
type Velocity = Vector

fallingBall :: Location -> Velocity -> SF() (Location, Velocity)
fallingBall location velocity = proc () -> do
    v <- ((y velocity)+)^<< integral -< -9.81 -- gravity
    y <- ((y location)+)^<< integral -< v
    returnA -< (Vector 0.0 y, Vector 0.0 v)

fallingBall' :: Location -> Velocity -> SF()((Location, Velocity), Event(Location, Velocity))
fallingBall' location velocity = proc () -> do
    yv@(loc, _) <- fallingBall location velocity -< ()
    hit       <- edge              -< y loc <= 0
    returnA -< (yv, hit `tag` yv)

bouncingBall :: Location -> SF()(Location, Velocity)
bouncingBall location = bbAux location (Vector 0.0 0.0)
    where bbAux location velocity = switch(fallingBall' location velocity) $ \(y, (Vector vX vY)) -> bbAux y (Vector vX (-vY))
 
main :: IO ()
main = do
    t <- getCurrentTime
    timeRef <- newIORef t
    let init        = putStrLn "Bouncing Ball:"
        actuate x (pos, vel) = when x (putStrLn ("pos: " ++ show pos ++ " | vel: " ++ show vel)) >> return False
        sense   _   = do
            now      <- getCurrentTime
            lastTime <- readIORef timeRef
            writeIORef timeRef now
            let dt = now `diffUTCTime` lastTime
            return (realToFrac dt, Just ())
    reactimate init sense actuate $ bouncingBall (Vector 0.0 1.0)
    