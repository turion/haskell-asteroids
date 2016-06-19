{-# LANGUAGE Arrows #-}
module UI (
    Input(..),
    UserInput(..),
    parseInput
) where

import FRP.Yampa

import Graphics.UI.GLUT

import Datatypes

data Input = Keyboard { 
    key       :: Key,
    keyState  :: KeyState,
    modifiers :: Modifiers
}

data UserInput = UserInput {
    acceleration :: Acceleration,
    turn         :: Orientation
}


-- TODO make in to two functions
--parseInput :: Event Input ->                                    (Acceleration, Orientation)
--parseInput    (Event (Keyboard (SpecialKey KeyUp)    (Down) _)) =  (  1.0 ,    0.0 )
--parseInput    (Event (Keyboard (SpecialKey KeyDown)  (Down) _)) =  ((-1.0),    0.0 )
--parseInput    (Event (Keyboard (SpecialKey KeyRight) (Down) _)) =  (  0.0 , (-1.0))
--parseInput    (Event (Keyboard (SpecialKey KeyLeft)  (Down) _)) =  (  0.0 ,   1.0 )
--parseInput    _                                                 =  (  0.0 ,    0.0 )



parseInput :: UserInput -> Event Input -> UserInput
parseInput    oldInput     newInput       = UserInput (parseAcceleration oldInput newInput) (parseOrientation oldInput newInput)       



parseAcceleration :: UserInput -> Event Input ->                                    Acceleration
parseAcceleration    _            (Event (Keyboard (SpecialKey KeyUp)    (Down) _)) =  1.0
parseAcceleration    _            (Event (Keyboard (SpecialKey KeyDown)  (Down) _)) =  (-1.0)
parseAcceleration    _            (Event (Keyboard (SpecialKey KeyUp)    (Up)   _)) =  0.0
parseAcceleration    _            (Event (Keyboard (SpecialKey KeyDown)  (Up)   _)) =  0.0
parseAcceleration    oldInput     _                                                 =  acceleration oldInput

parseOrientation :: UserInput -> Event Input ->                                    Orientation
parseOrientation    _            (Event (Keyboard (SpecialKey KeyRight) (Down) _)) =  (-1.0)
parseOrientation    _            (Event (Keyboard (SpecialKey KeyLeft)  (Down) _)) =  1.0
parseOrientation    _            (Event (Keyboard (SpecialKey KeyRight) (Up)   _)) =  0.0
parseOrientation    _            (Event (Keyboard (SpecialKey KeyLeft)  (Up)   _)) =  0.0
parseOrientation    oldInput     _                                                 =  turn oldInput
