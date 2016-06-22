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


-- Logic

movingShip :: GameObject ->                                             SF GameInput GameLevel
movingShip    (GameObject location velocity orientation gameObjectType) = proc (GameInput acceleration deltaOrientation) -> do
    dO <- (orientation+)  ^<< integral -< deltaOrientation
    vel <- (velocity ^+^) ^<< integral -< acceleration *^ Vector (-sin dO) (cos dO)
    loc <- (location ^+^) ^<< integral -< vel
    returnA -< GameLevel (GameObject loc vel dO gameObjectType) [] [] [] []

createShip :: Location -> SF GameInput GameLevel
createShip    location    = movingShip $ GameObject location (Vector 0.0 0.0) 0.0 Ship

-- Main

main :: IO ()
main    = do
    input <- newIORef (GameInput 0.0 0.0)
    output <- newIORef (GameLevel (GameObject (Vector 0.0 0.0) (Vector 0.0 0.0) 0.0 Ship) [] [] [] [])
    t <- getCurrentTime
    time <- newIORef t
    initGL
    handle <- reactInit (return (GameInput 0.0 0.0)) (actuator output) $ createShip (Vector 0.0 0.0)
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> handleInput input $ Event $ Keyboard key keyState modifiers)
    idleCallback $= Just (idle input time handle)
    displayCallback $= (readIORef output >>= renderLevel)
    t' <- getCurrentTime
    writeIORef time t'
    reshapeCallback $= Just reshape
    fullScreen
    mainLoop


idle :: IORef GameInput -> IORef UTCTime -> ReactHandle GameInput GameLevel -> IO()
idle    gameInput          time             handle                              = do
    input <- readIORef gameInput
    now <- getCurrentTime
    before <- readIORef time
    let deltaTime = realToFrac $ diffUTCTime now before
    _ <- react handle (deltaTime, Just input)
    writeIORef time now
    postRedisplay Nothing    


actuator :: IORef GameLevel -> ReactHandle GameInput GameLevel -> Bool -> GameLevel -> IO Bool
actuator    output              _                                   _       gameLevel    = do
    writeIORef output gameLevel
    return False
