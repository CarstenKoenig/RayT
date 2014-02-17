-- | provides a datatype `UnitDouble` that are doubles bound between 0 and 1
module RayT.Lights
    ( Light (..)
	, module RayT.Colors
	, module RayT.Vector
    ) where

import RayT.Colors
import RayT.Vector(N3, R3)

data Light
    = AmbientLight     Color
    | DirectionalLight N3 Color
    | PositionalLight  R3 Color