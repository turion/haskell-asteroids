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

movingShip :: GameObject ->                                             SF GameInput GameObject
movingShip    (GameObject location velocity orientation gameObjectType) = proc (GameInput acceleration deltaOrientation) -> do
    dO <- (orientation+)  ^<< integral -< deltaOrientation
    vX <- ((getX velocity)+) ^<< integral -< (-sin dO) * acceleration
    vY <- ((getY velocity)+) ^<< integral -< cos dO * acceleration
    x  <- ((getX location)+) ^<< integral -< vX
    y  <- ((getY location)+) ^<< integral -< vY
    returnA -< GameObject (Vector x y) (Vector vX vY) dO gameObjectType

createShip :: Location -> SF GameInput GameObject
createShip    location    = movingShip $ GameObject location (Vector 0.0 0.0) 0.0 Ship


-- Graphics

idle :: IORef GameInput -> IORef UTCTime -> ReactHandle GameInput GameObject -> IO()
idle    gameInput          time             handle                              = do
    input <- readIORef gameInput
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
    createWindow       "Haskelloids!"
    return ()
    
 
render :: GameObject -> IO()
render    gameObject    = do
    clear[ColorBuffer]
    preservingMatrix $ do
        let location = getLocation gameObject
        translate $ (Vector3 (realToFrac (getX location):: GLfloat) (realToFrac (getY location):: GLfloat) 0)
        let orientation = getOrientation gameObject
        rotate (realToFrac orientation * 360 / (2 * pi) :: GLfloat) $ Vector3 0 0 1
        renderPrimitive Polygon $ do
            vertex $ (Vertex3   0.00    0.05  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.02  (-0.02) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.00    0.00  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.02) (-0.02) 0 :: Vertex3 GLfloat)
    swapBuffers


-- Main

main :: IO ()
main    = do
    input <- newIORef (GameInput 0.0 0.0)
    output <- newIORef (GameObject (Vector 0.0 0.0) (Vector 0.0 0.0) 0.0 Ship)
    t <- getCurrentTime
    time <- newIORef t
    initGL
    handle <- reactInit (return (GameInput 0.0 0.0)) (actuator output) $ createShip (Vector 0.0 0.0)
    keyboardMouseCallback $= Just (\key keyState modifiers _ -> handleInput input $ Event $ Keyboard key keyState modifiers)
    idleCallback $= Just (idle input time handle)
    displayCallback $= (readIORef output >>= render)
    t' <- getCurrentTime
    writeIORef time t'
    mainLoop

actuator :: IORef GameObject -> ReactHandle GameInput GameObject -> Bool -> GameObject -> IO Bool
actuator    output              _                                   _       gameObject    = do
    writeIORef output gameObject
    return False
