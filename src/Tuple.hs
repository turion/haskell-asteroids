module Tuple (
    uncurry3
) where

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3    function  triple    = function (first triple) (second triple) (third triple) where
    first :: (a, b, c) -> a
    first (a, b, c)     = a
    second :: (a, b, c) -> b
    second (a, b, c)     = b
    third :: (a, b, c) -> c
    third (a, b, c)     = c