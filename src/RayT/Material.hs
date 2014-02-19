module RayT.Material
    ( Material (..)
    , module RayT.Colors
    ) where

import RayT.Colors

data Material = Mat
    { matColor :: Color 
    } deriving Show

