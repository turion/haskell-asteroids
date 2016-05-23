-- make module

data Vector = Vector { 
    x :: Double, 
    y :: Double
} deriving (Show)

-- e.g. Weapon or Shield useful for ship

data Ship = Ship{
    position :: Vector,
    velocity :: Vector
} deriving (Show)


-- for test purposes only
main :: IO()
main = do
    putStrLn ("Our ship:" ++ show ship)
        where position = (Vector 1.0 2.0)
              velocity = (Vector 0.4 0.3)
              ship = (Ship position velocity)