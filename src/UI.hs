{-# LANGUAGE Arrows #-}
module UI (
    KeyboardInput(..),
    UserInput(..),
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

data UserInput = NoInput | UserInput {
    acceleration :: Acceleration,
    turn         :: Orientation
}

data OtherButtons = OtherButtons {
  pause :: Bool,
  reset :: Bool
}

handleInput :: Window -> IORef GameState -> IORef Bool -> IORef UserInput -> Event KeyboardInput -> IO ()
handleInput    window    _ _   _           (Event (KeyboardInput (Char 'q') (Down) _))  = destroyWindow window
handleInput    window    _ resetTriggered _               (Event (KeyboardInput (Char 'r') (Down) _))  = do
  writeIORef resetTriggered True
  return ()
handleInput    window  gameState  _ _                (Event (KeyboardInput (Char ' ') (Down) _))  = do
  gs <- readIORef gameState
  let newShield | shields gs - 20 < 0 = 0
                | otherwise = shields gs - 20
  let isShieldOn | newShield == 0 = False
                 | otherwise = True
  writeIORef gameState $ GameState (level gs) (lifeCount gs) (score gs) newShield isShieldOn
  return ()
handleInput    window  gameState  _ _                (Event (KeyboardInput (Char ' ') (Up) _))  = do
  gs <- readIORef gameState
  writeIORef gameState $ GameState (level gs) (lifeCount gs) (score gs) (shields gs) False
  return ()
handleInput    _ _  _    gameInput       userInput        = do
    oldInput <- readIORef gameInput
    writeIORef gameInput $ UserInput (parseAcceleration oldInput userInput) (parseOrientation oldInput userInput)
    return ()

parseAcceleration :: UserInput -> Event KeyboardInput ->                                    Acceleration
parseAcceleration    _            (Event (KeyboardInput (SpecialKey KeyUp)    (Down) _)) =  1.0
parseAcceleration    _            (Event (KeyboardInput (SpecialKey KeyDown)  (Down) _)) =  (-1.0)
parseAcceleration    _            (Event (KeyboardInput (SpecialKey KeyUp)    (Up)   _)) =  0.0
parseAcceleration    _            (Event (KeyboardInput (SpecialKey KeyDown)  (Up)   _)) =  0.0
parseAcceleration    oldInput     _                                                 =  acceleration oldInput

parseOrientation :: UserInput -> Event KeyboardInput ->                                    Orientation
parseOrientation    _            (Event (KeyboardInput (SpecialKey KeyRight) (Down) _)) =  (-1.0)
parseOrientation    _            (Event (KeyboardInput (SpecialKey KeyLeft)  (Down) _)) =  1.0
parseOrientation    _            (Event (KeyboardInput (SpecialKey KeyRight) (Up)   _)) =  0.0
parseOrientation    _            (Event (KeyboardInput (SpecialKey KeyLeft)  (Up)   _)) =  0.0
parseOrientation    oldInput     _                                                 =  turn oldInput
