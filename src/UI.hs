{-# LANGUAGE Arrows #-}
module UI (
    UserInput(..),
    GameInput(..),
    handleInput
) where

import FRP.Yampa

import Graphics.UI.GLUT
import Data.IORef

import Datatypes

data UserInput = Keyboard { 
    key       :: Key,
    keyState  :: KeyState,
    modifiers :: Modifiers
}

data GameInput = GameInput {
    acceleration :: Acceleration,
    turn         :: Orientation
}


handleInput :: IORef GameInput -> Event UserInput -> IO ()
handleInput    gameInput       userInput       = do
    oldInput <- readIORef gameInput
    writeIORef gameInput $ GameInput (parseAcceleration oldInput userInput) (parseOrientation oldInput userInput)
    return ()    

parseAcceleration :: GameInput -> Event UserInput ->                                    Acceleration
parseAcceleration    _            (Event (Keyboard (SpecialKey KeyUp)    (Down) _)) =  1.0
parseAcceleration    _            (Event (Keyboard (SpecialKey KeyDown)  (Down) _)) =  (-1.0)
parseAcceleration    _            (Event (Keyboard (SpecialKey KeyUp)    (Up)   _)) =  0.0
parseAcceleration    _            (Event (Keyboard (SpecialKey KeyDown)  (Up)   _)) =  0.0
parseAcceleration    oldInput     _                                                 =  acceleration oldInput

parseOrientation :: GameInput -> Event UserInput ->                                    Orientation
parseOrientation    _            (Event (Keyboard (SpecialKey KeyRight) (Down) _)) =  (-1.0)
parseOrientation    _            (Event (Keyboard (SpecialKey KeyLeft)  (Down) _)) =  1.0
parseOrientation    _            (Event (Keyboard (SpecialKey KeyRight) (Up)   _)) =  0.0
parseOrientation    _            (Event (Keyboard (SpecialKey KeyLeft)  (Up)   _)) =  0.0
parseOrientation    oldInput     _                                                 =  turn oldInput
