module RayT (
	R3
) where

import RayT.Vector
import RayT.Image

type Width  = Double
type Height = Double

data Screen = Screen
	{ center :: R3
    , axisX  :: R3
    , axisY  :: R3
	} deriving Show

data Camera = Camera 
	{ eyePos :: R3
	, screen :: Screen
	} deriving Show

newtype Ray = Ray (R3, R3)

-- | a default screen is located at the origin (0,0,0) with default axis
defaultScreen :: (Width, Height) -> Screen
defaultScreen (w, h) = Screen 0 (w.*bX) (h.*bY)

-- | default camera uses the default screen with the cameras eye located
-- at the given distance along the negative Z-axis 
-- (so the camera will look along the positive Z-axis)
defaultCamera :: Double -> (Width, Height) -> Camera
defaultCamera dist2Cent = Camera (negate dist2Cent .* bZ) . defaultScreen

ray :: R3 -> R3 -> Ray
ray start dir = Ray (start, dir)

-- | ray from from the first point in the direction to the second
rayTo :: R3 -> R3 -> Ray
rayTo from to = ray from (to - from)

-- | determine a point on the screen by rastering the complete screen
rasterPoint :: Screen -> ImageSize -> ImageCoords -> R3
rasterPoint screen (iW, iH) (iX, iY) = start + x.*aX + y.*aY
	where x = 0.5 + fromIntegral iX / fromIntegral iW
	      y = 0.5 + fromIntegral iY / fromIntegral iH
	      aX = axisX screen
	      aY = axisY screen
	      start = center screen - 0.5.*(aX + aY)

-- | uses `rasterPoint` to get a start-ray from the camaras eye to a rasterized point on the screen
rasterRay :: Camera -> ImageSize -> ImageCoords -> Ray
rasterRay cam sz = rayTo (eyePos cam) . rasterPoint (screen cam) sz