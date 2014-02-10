-- | some primitive objects
module RayT.Primitives
    ( Radius
    , sphere
    ) where

import Control.Applicative ((<$>))

import RayT
import RayT.Vector

type Radius = Double

sphere :: Material -> R3 -> Radius -> Object
sphere mat cent r ray =
    let s  = rayStart ray
        d  = rayDirection ray
        o  = s - cent
        a  = vLen2 d
        b  = 2 * (o .*. d)
        c  = vLen2 o - r*r
        pointAt t  = s + t.*d
        resultAt t = let pt = pointAt t
                         n  = vNorm (pt - cent)
                     in Intersection t pt n mat
    in case solveQ a b c of
        []       -> Nothing
        [t]      -> Just $ resultAt t
        [t1, t2] -> if 0 <= t1 && t1 <= t2 
                    then Just $ resultAt t1
                    else if 0 <= t2
                    then Just $ resultAt t2
                    else Nothing
    


solveQ :: Double -> Double -> Double -> [Double]
solveQ a b c
    | a ~= 0 && b ~= 0 = []
    | a ~= 0    = [negate c / b]
    | det < 0   = []
    | det ~= 0  = [negate b / a]
    | otherwise = [(negate b + sdet)/a, (negate b - sdet)/a]
    where det  = b*b - a*c
          sdet = sqrt det