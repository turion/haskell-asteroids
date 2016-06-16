{-# LANGUAGE Arrows #-}
module UI (
    Input(..),
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

parseInput :: Event Input ->                                      (Acceleration, Orientation)
parseInput    (Event (Keyboard (SpecialKey KeyUp)    (Down) _))   =  ((Vector   0.0   1.0 ),   0.0 )
parseInput    (Event (Keyboard (SpecialKey KeyDown)  (Down) _))   =  ((Vector   0.0 (-1.0)),   0.0 )
parseInput    (Event (Keyboard (SpecialKey KeyRight) (Down) _))   =  ((Vector   0.0   0.0 ),   1.0 )
parseInput    (Event (Keyboard (SpecialKey KeyLeft)  (Down) _))   =  ((Vector   0.0   0.0 ), (-1.0))
parseInput    _                                                   =  ((Vector   0.0   0.0 ),   0.0 )
