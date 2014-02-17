module RayT.Tracing
    ( traceRay
    , module RayT.Scene
    ) where

import RayT.Scene

-- | traces a single ray through a scene
-- using the easiest possible strategy (brute force) right now
traceRay :: R3 -> Scene -> Ray -> Color
traceRay eP scene = maybe black (calcShading eP scene) . findIntersection scene

-- | basic shading algorithm for an intersections
calcShading :: R3 -> Scene -> Intersection -> Color
calcShading eP s i = lightF * mc
    where mc     = matColor . iMaterial $ i
          lightF = sum . map (calcLight eP s i) $ lights s

calcLight :: R3 -> Scene -> Intersection -> Light -> Color
calcLight _ _ _ (AmbientLight a)               = a
calcLight _ _ i (DirectionalLight (Norm3 d) l) = scale (negate (d.*.n)) l
    where Norm3 n = iNormal i
calcLight _ s i (PositionalLight lp lc)        = if inSight s lp (iPoint i)
                                                 then lc else black