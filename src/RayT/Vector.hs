{-# LANGUAGE BangPatterns #-}

module RayT.Vector
    ( Vector3 (Vec3)
    , Normal3 (Norm3)
    , R3, N3
    , vLen2, vLength
    , vNorm, normal
    , (.*)
    , (.*.)
    , bX, bY, bZ
    ) where

import RayT.Utils

newtype Vector3 a = Vec3 (a, a, a)
type    R3        = Vector3 Double

newtype Normal3 a = Norm3 (Vector3 a)   
type    N3        = Normal3 Double

bX :: Num a => Vector3 a
bX = Vec3 (1,0,0)

bY :: Num a => Vector3 a
bY = Vec3 (0,1,0)

bZ :: Num a => Vector3 a
bZ = Vec3 (0,0,1)

vLength :: Floating a => Vector3 a -> a
vLength = sqrt . vLen2

vLen2 :: Floating a => Vector3 a -> a
vLen2 (Vec3 (!a,!b,!c)) = a*a + b*b + c*c

vNorm :: Floating a => Vector3 a -> Vector3 a
vNorm v = (1/vLength v) .* v

normal :: Floating a => Vector3 a -> Normal3 a
normal v = Norm3 (vNorm v)

-- * basic instances

instance (Ord a, Fractional a) => Eq (Vector3 a) where
    (Vec3 (a,b,c)) == (Vec3 (a',b',c')) =
        a ~= a' &&
        b ~= b' &&
        c ~= c'

instance Show a => Show (Vector3 a) where
    show (Vec3 (a,b,c)) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

instance Show a => Show (Normal3 a) where
    show (Norm3 v) = show v

instance (Ord a, Fractional a) => Eq (Normal3 a) where
    Norm3 v == Norm3 v' = v == v'

-- * operators

infix 7 .*
(.*) :: Num a => a -> Vector3 a -> Vector3 a
(!s) .* (Vec3 (!a, !b, !c)) = Vec3 (s*a, s*b, s*c)

infix 6 .*.
(.*.) :: Num a => Vector3 a -> Vector3 a -> a
(Vec3 (!a,!b,!c)) .*. (Vec3 (!a',!b',!c')) = a*a' + b*b' + c*c'

instance Num a => Num (Vector3 a) where
    (Vec3 (!a,!b,!c)) + (Vec3 (!a',!b',!c')) = Vec3 (a+a', b+b', c+c')
    (Vec3 (!a,!b,!c)) * (Vec3 (!a',!b',!c')) = Vec3 (a*a', b*b', c*c')
    (Vec3 (!a,!b,!c)) - (Vec3 (!a',!b',!c')) = Vec3 (a-a', b-b', c-c')
    negate (Vec3 (!a,!b,!c))  = Vec3 (negate a, negate b, negate c)
    fromInteger !a            = Vec3 (fromInteger a, fromInteger a, fromInteger a)
    abs _                     = undefined
    signum _                  = undefined