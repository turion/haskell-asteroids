{-
    Datatypes.hs
    Chris, Liv Wilczewski
    May 2016
-}
-- make module

type Vec = (Double, Double)

data Vector = Vector Vec
    deriving Show
    
-- data Vector = Vector { 
    -- x :: Double, 
    -- y :: Double
-- } deriving (Show)

-- e.g. Weapon or Shield useful for ship

type Position = Vector
type Velocity = Vector

data Object = Object Position Velocity
    deriving Show

-- data Object = Object{
    -- position :: Vector,
    -- velocity :: Vector
-- } deriving (Show)

type User = Object
type Enemy = Object
type Asteroid = Object

data Level = Level User [Enemy] [Asteroid]
    deriving Show
-- data Level = Level{
    -- user :: Object,
    -- enemy :: [Object],
    -- asteroid :: [Object]
-- } deriving Show

data Game = Game [Level]
    deriving Show

-- for test purposes only
main :: IO()
main = do
    putStrLn ("Our ship:" ++ show ship ++ "Level:" ++ show level)
        where position = (Vector (1.0,2.0))
              velocity = (Vector (0.4,0.3))
              ship = (Object position velocity)
              user = (Object position velocity)
              enemy = [(Object position velocity)]
              asteroid = [(Object position velocity)]
              level = (Level user enemy asteroid)