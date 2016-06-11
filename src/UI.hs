{-# LANGUAGE Arrows #-}
module UI (Input(..)) where

import FRP.Yampa

import Graphics.UI.GLUT

data Input = Keyboard { 
    key       :: Key,
    keyState  :: KeyState,
    modifiers :: Modifiers }

--data ParsedInput = ParsedInput { 
--    accelerationEvents       :: Event Input, 
--    stopAccelerationEvents   :: Event Input, 
--    deaccelerationEvents     :: Event Input, 
--    stopDeaccelerationEvents :: Event Input, 
--    turnrightEvents          :: Event Input, 
--    stopTurnrightEvents      :: Event Input, 
--    turnleftEvents           :: Event Input,
--    stopTurnleftEvents       :: Event Input }

filterKeyState :: KeyState -> SF (Event Input) (Event Input)
filterKeyState ks = arr $ filterE ((==ks) . keyState)

filterKey :: Key -> SF (Event Input) (Event Input)
filterKey k = arr $ filterE ((==k) . key)
                       
--parseInput :: SF (Event Input) ParsedInput
--parseInput = proc input -> do
--    keyStateDownEvents      <- filterKeyState (Down)           -< input
--    keyStateUpEvents        <- filterKeyState (Up)             -< input
--    keyUpInitEvents         <- filterKey (SpecialKey KeyUp)    -< keyStateDownEvents
--    keyDownInitEvents       <- filterKey (SpecialKey KeyDown)  -< keyStateDownEvents
--    keyRightInitEvents      <- filterKey (SpecialKey KeyRight) -< keyStateDownEvents
--    keyLeftInitEvents       <- filterKey (SpecialKey KeyLeft)  -< keyStateDownEvents
--    keyUpReleaseEvents      <- filterKey (SpecialKey KeyUp)    -< keyStateUpEvents
--    keyDownReleaseEvents    <- filterKey (SpecialKey KeyDown)  -< keyStateUpEvents
--    keyRightReleaseEvents   <- filterKey (SpecialKey KeyRight) -< keyStateUpEvents
--    keyLeftReleaseEvents    <- filterKey (SpecialKey KeyLeft)  -< keyStateUpEvents
--    returnA -< ParsedInput keyUpInitEvents keyUpReleaseEvents keyDownInitEvents keyDownReleaseEvents keyRightInitEvents keyRightReleaseEvents keyLeftInitEvents keyLeftReleaseEvents
