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


-- TODO make in to two functions
parseInput :: Event Input ->                                      (Acceleration, Orientation)
parseInput    (Event (Keyboard (SpecialKey KeyUp)    (Down) _))   =  (  1.0 ,    0.0 )
parseInput    (Event (Keyboard (SpecialKey KeyDown)  (Down) _))   =  ((-1.0),    0.0 )
parseInput    (Event (Keyboard (SpecialKey KeyRight) (Down) _))   =  (  0.0 , (-1.0))
parseInput    (Event (Keyboard (SpecialKey KeyLeft)  (Down) _))   =  (  0.0 ,   1.0 )
parseInput    _                                                   =  (  0.0 ,    0.0 )
