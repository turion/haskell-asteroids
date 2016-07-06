{-# LANGUAGE Arrows #-}
module UI (
    KeyboardInput(..),
    GameInput(..),
    handleInput
) where

import FRP.Yampa

import Graphics.UI.GLUT
import Data.IORef

import Datatypes

data KeyboardInput = KeyboardInput {
    key       :: Key,
    keyState  :: KeyState,
    modifiers :: Modifiers
}

data GameInput = GameInput {
    acceleration :: Acceleration,
    turn         :: Orientation
}


handleInput :: Window -> IORef GameInput -> Event KeyboardInput -> IO ()
handleInput    window    _               (Event (KeyboardInput (Char 'q') (Down) _)) = destroyWindow window
handleInput    _         gameInput       userInput       = do
    oldInput <- readIORef gameInput
    writeIORef gameInput $ GameInput (parseAcceleration oldInput userInput) (parseOrientation oldInput userInput)
    return ()

parseAcceleration :: GameInput -> Event KeyboardInput ->                                    Acceleration
parseAcceleration    _            (Event (KeyboardInput (SpecialKey KeyUp)    (Down) _)) =  1.0
parseAcceleration    _            (Event (KeyboardInput (SpecialKey KeyDown)  (Down) _)) =  (-1.0)
parseAcceleration    _            (Event (KeyboardInput (SpecialKey KeyUp)    (Up)   _)) =  0.0
parseAcceleration    _            (Event (KeyboardInput (SpecialKey KeyDown)  (Up)   _)) =  0.0
parseAcceleration    oldInput     _                                                 =  acceleration oldInput

parseOrientation :: GameInput -> Event KeyboardInput ->                                    Orientation
parseOrientation    _            (Event (KeyboardInput (SpecialKey KeyRight) (Down) _)) =  (-1.0)
parseOrientation    _            (Event (KeyboardInput (SpecialKey KeyLeft)  (Down) _)) =  1.0
parseOrientation    _            (Event (KeyboardInput (SpecialKey KeyRight) (Up)   _)) =  0.0
parseOrientation    _            (Event (KeyboardInput (SpecialKey KeyLeft)  (Up)   _)) =  0.0
parseOrientation    oldInput     _                                                 =  turn oldInput
