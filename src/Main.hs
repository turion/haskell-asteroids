{-# LANGUAGE Arrows #-}

import Control.Monad
import Control.Arrow
import Data.IORef
import Data.List
import Data.Time.Clock
import FRP.Yampa
import System.Random
-- Graphics
import FRP.Yampa.Simulation
import Graphics.UI.GLUT
import Control.Concurrent
-- Haskelloids
import Datatypes
import UI
import Graphics
import Physics
import Generator
import Game


-- Main

main :: IO ()
main    = do
    window <- initGL
    input <- newIORef (UserInput 0.0 0.0 False)
    output <- newIORef (EmptyLevel)
    fullScreen
    reshapeCallback $= Just reshape
    showText "Haskelloids" (Vector (-0.55) 0) [0.5, 0.0, 0.5] 0.002 Title
    threadDelay 2000000
    t <- getCurrentTime
    time <- newIORef t
    startTime <- newIORef t
    randomGenerator <- getStdGen
    let level = generateLevel 5 10 randomGenerator
    gameState <- newIORef $ GameState 1 3 0 1000 False
    resetTriggered <- newIORef False
    handle <- reactInit (return (UserInput 0.0 0.0 False)) (actuator output) $ game level
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> handleInput window gameState resetTriggered input $ Event $ KeyboardInput key keyState modifiers)
    idleCallback $= Just (idle input time handle)
    displayCallback $= (drawScreen gameState output startTime)
    mainLoop

idle :: IORef UserInput -> IORef UTCTime -> ReactHandle UserInput GameLevel -> IO()
idle    userInput          time             handle                             = do
    input <- readIORef userInput
    now <- getCurrentTime
    before <- readIORef time
    let deltaTime = realToFrac $ diffUTCTime now before
    _ <- react handle (deltaTime, Just input)
    writeIORef time now
    postRedisplay Nothing

actuator :: IORef GameLevel -> ReactHandle UserInput GameLevel -> Bool -> GameLevel -> IO Bool
actuator    output             _                                  _       gameLevel    = do
    writeIORef output gameLevel
    return False
