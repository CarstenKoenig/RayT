{-# LANGUAGE BangPatterns #-}

module RayT.Vector
    ( Vector3 (Vec3)
    , R3
    , vLen2, vLength
    , vNorm
    , (.*)
    , (.*.)
    , (~=)
    ) where

newtype Vector3 a = Vec3 (a, a, a)
type    R3        = Vector3 Double

vLength :: Floating a => Vector3 a -> a
vLength = sqrt . vLen2

vLen2 :: Floating a => Vector3 a -> a
vLen2 (Vec3 (a,b,c)) = a*a + b*b + c*c

vNorm :: Floating a => Vector3 a -> Vector3 a
vNorm v = (1/vLength v) .* v

-- * basic instances

instance (Ord a, Fractional a) => Eq (Vector3 a) where
    (Vec3 (a,b,c)) == (Vec3 (a',b',c')) =
        a ~= a' &&
        b ~= b' &&
        c ~= c'

instance Show a => Show (Vector3 a) where
    show (Vec3 (a,b,c)) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

-- * operators

infix 5 ~=
(~=) :: (Ord a, Fractional a) => a -> a-> Bool
a ~= b
    | abs b < tolerance = abs a < tolerance
    | otherwise         = abs ((a-b)/b) < tolerance

infix 7 .*
(.*) :: Num a => a -> Vector3 a -> Vector3 a
(!s) .* (Vec3 (!a, !b, !c)) = Vec3 (s*a, s*b, s*c)

infix 6 .*.
(.*.) :: Num a => Vector3 a -> Vector3 a -> a
(Vec3 (a,b,c)) .*. (Vec3 (a',b',c')) = a*a' + b*b' + c*c'

instance Num a => Num (Vector3 a) where
    (Vec3 (!a,!b,!c)) + (Vec3 (!a',!b',!c')) = Vec3 (a+a', b+b', c+c')
    (Vec3 (!a,!b,!c)) - (Vec3 (!a',!b',!c')) = Vec3 (a-a', b-b', c-c')
    negate (Vec3 (!a,!b,!c))  = Vec3 (negate a, negate b, negate c)
    fromInteger !a            = Vec3 (fromInteger a, fromInteger a, fromInteger a)
    _ * _                     = undefined
    abs _                     = undefined
    signum _                  = undefined


-- * helpers
tolerance :: Fractional a => a
tolerance = 0.00001