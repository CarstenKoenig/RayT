{-# LANGUAGE BangPatterns #-}

module RayT.Vector
    ( Vector3 (Vec3)
    , R3
    , (.*)
    ) where

newtype Vector3 a = Vec3 (a, a, a)

type    R3        = Vector3 Double

tolerance :: Fractional a => a
tolerance = 0.00001

near :: (Ord a, Fractional a) => a -> a -> Bool
a `near`b
    | abs b < tolerance = abs a < tolerance
    | otherwise         = abs ((a-b)/b) < tolerance

instance (Ord a, Fractional a) => Eq (Vector3 a) where
    (Vec3 (a,b,c)) == (Vec3 (a',b',c')) =
        a `near` a' &&
        b `near` b' &&
        c `near` c'

instance Show a => Show (Vector3 a) where
    show (Vec3 (a,b,c)) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

infix 7 .*
(.*) :: Num a => a -> Vector3 a -> Vector3 a
(!s) .* (Vec3 (!a, !b, !c)) = Vec3 (s*a, s*b, s*c)

instance Num a => Num (Vector3 a) where
    (Vec3 (!a,!b,!c)) + (Vec3 (!a',!b',!c')) = Vec3 (a+a', b+b', c+c')
    (Vec3 (!a,!b,!c)) - (Vec3 (!a',!b',!c')) = Vec3 (a-a', b-b', c-c')
    negate (Vec3 (!a,!b,!c))  = Vec3 (negate a, negate b, negate c)
    fromInteger !a            = Vec3 (fromInteger a, fromInteger a, fromInteger a)
    _ * _                     = undefined
    abs _                     = undefined
    signum _                  = undefined


