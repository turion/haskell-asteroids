-- this function gives true if the enemy should turn in clockwise direction, false otherwise
-- takes coordinates of the ship, coordinates of an enemy and the angle to what the enemy is facing
rotateClockwiseToAim :: Int -> Int -> Int -> Int -> Float -> Bool
rotateClockwiseToAim x1 y1 x2 y2 angle | x1 == x2 && y1 == y2 = True
                      | phi < pi && angle < phi = False
                      | phi < pi && angle > pi + phi = False
                      | phi > pi && angle > (phi - pi) && angle < phi = False
                      | otherwise = True
    where
      phi = calculateAngle x1 y1 x2 y2

calculateAngle :: Int -> Int -> Int -> Int -> Float
calculateAngle x1 y1 x2 y2 | x1 == x2 && y1 == y2 = 0
                       | x1 == x2 && y1 < y2 = 3*pi/2
                       | x1 == x2 && y1 > y2 = pi/2
                       | y1 == y2 && x1 > x2 = 0
                       | y1 == y2 && x1 < x2 = pi
                       | y1 > y2 && x1 > x2 = atan((fromIntegral y1 - fromIntegral y2)/(fromIntegral x1 - fromIntegral x2))
                       | y1 > y2 && x1 < x2 = pi/2 + atan((fromIntegral x2 - fromIntegral x1)/(fromIntegral y1 - fromIntegral y2))
                       | y1 < y2 && x1 < x2 = pi + atan((fromIntegral y2 - fromIntegral y1)/(fromIntegral x2 - fromIntegral x1))
                       | y1 < y2 && x1 > x2 = 3*pi/2 + atan((fromIntegral x1 - fromIntegral x2)/(fromIntegral y2 - fromIntegral y1))
                       | otherwise = 10           -- should not happen