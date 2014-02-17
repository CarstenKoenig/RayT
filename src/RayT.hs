module RayT 
    ( Width, Height
    , Screen (..)
    , Camera (..)
    , Scene (Scene)
    , Material (Mat)
    , Radius, sphere
    , defaultScreen, defaultCamera
    , traceToPng
    , module RayT.Vector
    , module RayT.Colors
    , module RayT.Lights
    ) where

import RayT.Vector
import RayT.Colors
import RayT.Lights
import RayT.Image
import RayT.Primitives


-- | a default screen is located at the origin (0,0,0) with default axis
defaultScreen :: (Width, Height) -> Screen
defaultScreen (w, h) = Screen 0 (w.*bX) (h.*bY)

-- | default camera uses the default screen with the cameras eye located
-- at the given distance along the negative Z-axis 
-- (so the camera will look along the positive Z-axis)
defaultCamera :: Double -> (Width, Height) -> Camera
defaultCamera dist2Cent = Camera ((negate . abs $ dist2Cent) .* bZ) . defaultScreen


