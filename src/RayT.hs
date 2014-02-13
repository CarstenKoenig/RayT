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
    , Scene (Scene)
    , R3, Vector3 (..)
    , rgb
    , defaultScreen, defaultCamera
    , ray, rayN, rayTo, rayStart, rayDirection
    , traceRay
    ) where

import GHC.Exts
import Data.Maybe (catMaybes)

import RayT.Utils
import RayT.Vector
import RayT.Colors
import RayT.Lights

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

newtype Ray = Ray (R3, N3)
    deriving (Eq, Show)

data Intersection = Intersection
    { iDistance :: Double
    , iPoint    :: R3
    , iNormal   :: N3
    , iMaterial :: Material 
    } deriving Show

data Material = Mat
    { matColor :: Color 
    } deriving Show

type Object = Ray -> Maybe Intersection
data Scene  = Scene
    { objects :: [Object]
    , lights  :: [Light]
    }

-- | a default screen is located at the origin (0,0,0) with default axis
defaultScreen :: (Width, Height) -> Screen
defaultScreen (w, h) = Screen 0 (w.*bX) (h.*bY)

-- | default camera uses the default screen with the cameras eye located
-- at the given distance along the negative Z-axis 
-- (so the camera will look along the positive Z-axis)
defaultCamera :: Double -> (Width, Height) -> Camera
defaultCamera dist2Cent = Camera ((negate . abs $ dist2Cent) .* bZ) . defaultScreen

rayN :: R3 -> N3 -> Ray
rayN start dir = Ray (start, dir)

ray :: R3 -> R3 -> Ray
ray start = rayN start . normal

-- | ray from from the first point in the direction to the second
rayTo :: R3 -> R3 -> Ray
rayTo from to = ray from (to - from)

rayStart :: Ray -> R3
rayStart (Ray (s, _)) = s

rayDirection :: Ray -> N3
rayDirection (Ray (_, d)) = d

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
calcLight _ _ _ (AmbientLight a)                  = a
calcLight _ _ i (DirectionalLight (Norm3 d) l) = scale (negate (d.*.n)) l
	where Norm3 n = iNormal i

findIntersection :: Scene -> Ray -> Maybe Intersection
findIntersection s r =
    case intersections of
        []   -> Nothing
        ints -> Just . head . sortWith iDistance . filter ((>~ 0) . iDistance) $ ints
    where intersections = catMaybes . map ((flip ($)) r) . objects $ s
