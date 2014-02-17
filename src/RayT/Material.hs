module RayT.Material
    ( Material (..)
    , Color 
    , rgb
    , module RayT.Colors
    ) where

import RayT.Colors

data Material = Mat
    { matColor :: Color 
    } deriving Show

