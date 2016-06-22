-- this function gives true if the enemy should turn in clockwise direction, false otherwise
aim :: Int -> Int -> Int -> Int -> Float -> Bool
aim x1 y1 x2 y2 angle | x1 == x2 && y1 == y2 = True
                      | phi < pi && angle < phi = False
                      | phi < pi && angle > pi + phi = False
                      | phi > pi && angle > (phi - pi) && angle < phi = False
                      | otherwise = True
    where
      phi = countAngle x1 y1 x2 y2

countAngle :: Int -> Int -> Int -> Int -> Float
countAngle x1 y1 x2 y2 | x1 == x2 && y1 == y2 = 0
                       | x1 == x2 && y1 < y2 = 3*pi/2
                       | x1 == x2 && y1 > y2 = pi/2
                       | y1 == y2 && x1 > x2 = 0
                       | y1 == y2 && x1 < x2 = pi
                       | y1 > y2 && x1 > x2 = atan((fromIntegral y1 - fromIntegral y2)/(fromIntegral x1 - fromIntegral x2))
                       | y1 > y2 && x1 < x2 = pi/2 + atan((fromIntegral x2 - fromIntegral x1)/(fromIntegral y1 - fromIntegral y2))
                       | y1 < y2 && x1 < x2 = pi + atan((fromIntegral y2 - fromIntegral y1)/(fromIntegral x2 - fromIntegral x1))
                       | y1 < y2 && x1 > x2 = 3*pi/2 + atan((fromIntegral x1 - fromIntegral x2)/(fromIntegral y2 - fromIntegral y1))
                       | otherwise = 10           -- should not happen