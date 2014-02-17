module RayT.Rays
    ( Ray
    , Intersection (..)
    , ray, rayN, rayTo, rayStart, rayDirection
    , module RayT.Material
    , module RayT.Vector
    ) where

import RayT.Material
import RayT.Vector

newtype Ray = Ray (R3, N3)
    deriving (Eq, Show)

data Intersection = Intersection
    { iDistance :: Double
    , iPoint    :: R3
    , iNormal   :: N3
    , iMaterial :: Material 
    } deriving Show

rayN :: R3 -> N3 -> Ray
rayN start dir = Ray (start, dir)

ray :: R3 -> R3 -> Ray
ray start = rayN start . normal

-- | ray from from the first point in the direction to the second
rayTo :: R3 -> R3 -> Ray
rayTo from to = ray from (to - from)

rayStart :: Ray -> R3
rayStart (Ray (s, _)) = s

rayDirection :: Ray -> N3
rayDirection (Ray (_, d)) = d