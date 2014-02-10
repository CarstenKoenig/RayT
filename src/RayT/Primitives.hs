-- | some primitive objects
module RayT.Primitives
    ( Radius
    , sphere
    ) where

import RayT
import RayT.Vector
import RayT.Utils

type Radius = Double

sphere :: Material -> R3 -> Radius -> Object
sphere mat cent rad ry =
    let s  = rayStart ry
        d  = rayDirection ry
        o  = s - cent
        a  = vLen2 d
        b  = 2 * (o .*. d)
        c  = vLen2 o - rad*rad
        resultAt t = let pt = s + t.*d
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
        _        -> error "unexpected case"
