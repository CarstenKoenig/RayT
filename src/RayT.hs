module RayT 
	( ImageCoords
    , ImageSize
    , Screen (..)
    , Camera (..)
    , Ray
    , Intersection (..)
    , Material (..)
    , Color 
    , Object
    , Scene
    , R3, Vector3 (..)
    , rgb
    , defaultScreen, defaultCamera
    , ray, rayTo, rayStart, rayDirection
    , traceRay
	) where

import GHC.Exts
import Data.Maybe (catMaybes)

import RayT.Vector
import RayT.Colors

type Width  = Double
type Height = Double

-- | Pixel-coordinate (X, Y)
type    ImageCoords   = (Int, Int)

-- | Image size in Pixels (width, height)
type    ImageSize     = (Int, Int)

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

data Intersection = Intersection
	{ iDistance :: Double
	, iPoint    :: R3
	, iNormal   :: R3
	, iMaterial :: Material 
	} deriving Show

data Material = Mat
	{ matColor :: Color 
	} deriving Show

type Object = Ray -> Maybe Intersection
type Scene  = [Object]

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

rayStart :: Ray -> R3
rayStart (Ray (s, _)) = s

rayDirection :: Ray -> R3
rayDirection (Ray (_, d)) = d

-- | traces a single ray through a scene
-- using the easiest possible strategy (brute force) right now
traceRay :: Scene -> Ray -> Color
traceRay scene r =
	case intersections of
		[]   -> black
		ints -> matColor . iMaterial . head . sortWith iDistance $ ints
	where intersections = catMaybes . map ((flip ($)) r) $ scene