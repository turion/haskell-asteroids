-- make module

data Vector = Vector { 
    x :: Double, 
    y :: Double
} deriving (Show)

-- e.g. Weapon or Shield useful for ship

data Object = Object{
    position :: Vector,
    velocity :: Vector
} deriving (Show)

data Level = Level{
    user :: Object,
    enemy :: [Object],
    asteroid :: [Object]
} deriving Show

type Game = ([Level])

-- for test purposes only
main :: IO()
main = do
    putStrLn ("Our ship:" ++ show ship ++ "Level:" ++ show level)
        where position = (Vector 1.0 2.0)
              velocity = (Vector 0.4 0.3)
              ship = (Object position velocity)
              user = (Object position velocity)
              enemy = [(Object position velocity)]
              asteroid = [(Object position velocity)]
              level = (Level user enemy asteroid)