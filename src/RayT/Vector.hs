module RayT.Vector
    ( Vector3 (Vec3)
    , R3
    ) where

newtype Vector3 a = Vec3 (a, a, a)
    deriving Eq

type    R3        = Vector3 Double

instance Show a => Show (Vector3 a) where
    show (Vec3 (a,b,c)) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

instance Num a => Num (Vector3 a) where
    (Vec3 (a,b,c)) + (Vec3 (a',b',c')) = Vec3 (a+a', b+b', c+c')
    (Vec3 (a,b,c)) - (Vec3 (a',b',c')) = Vec3 (a-a', b-b', c-c')
    negate (Vec3 (a,b,c))              = Vec3 (negate a, negate b, negate c)
    fromInteger a                      = Vec3 (fromInteger a, fromInteger a, fromInteger a)
    _ * _                              = undefined
    abs _                              = undefined
    signum _                           = undefined


