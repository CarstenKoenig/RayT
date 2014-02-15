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
    let s          = rayStart ry
        Norm3 d    = rayDirection ry
        o          = s - cent
        b          = 2 * (o .*. d)
        c          = vLen2 o - rad*rad
        resultAt t = let pt = s + t.*d
                         n  = normal (pt - cent)
                     in Intersection t pt n mat
    in case solveQ 1 b c of
        []       -> Nothing
        [t]      -> if t >~ 0
                    then Just $ resultAt t 
                    else Nothing
        [t1, t2] -> if t1 >~ 0 && t2 >~ t1
                    then Just $ resultAt t1
                    else if t2 >~ 0
                    then Just $ resultAt t2
                    else Nothing
        _        -> error "unexpected case"
