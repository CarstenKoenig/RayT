{-# LANGUAGE TemplateHaskell #-}

module RayT 
    ( ImageCoords
    , ImageSize
    , Screen, center, axisX, axisY
    , Camera, eyePos, screen
    , Ray
    , Material (..), matColor
    , Intersection (..), iDistance, iPoint, iNormal, iMaterial
    , Color 
    , Object
    , Scene (..), objects, lights
    , R3, Vector3 (..)
    , rgb
    , defaultScreen, defaultCamera
    , ray, rayN, rayTo, rayStart, rayDirection
    , traceRay
    ) where

import GHC.Exts

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Lens.TH

import Data.Maybe (catMaybes, isNothing)

import RayT.Vector
import RayT.Colors
import RayT.Lights

type Width            = Double
type Height           = Double

-- | Pixel-coordinate (X, Y)
type    ImageCoords   = (Int, Int)

-- | Image size in Pixels (width, height)
type    ImageSize     = (Int, Int)

data Screen = Screen
    { _center :: R3
    , _axisX  :: R3
    , _axisY  :: R3
    } deriving Show
makeLenses ''Screen

data Camera = Camera 
    { _eyePos :: R3
    , _screen :: Screen
    } deriving Show
makeLenses ''Camera

newtype Ray = Ray (R3, N3)
    deriving (Eq, Show)

data Material = Mat
    { _matColor :: Color 
    } deriving Show
makeLenses ''Material

data Intersection = Intersection
    { _iDistance :: Double
    , _iPoint    :: R3
    , _iNormal   :: N3
    , _iMaterial :: Material 
    } deriving Show
makeLenses ''Intersection

type Object = Ray -> Maybe Intersection
data Scene  = Scene
    { _objects :: [Object]
    , _lights  :: [Light]
    }
makeLenses ''Scene

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
	where mc     = i ^. iMaterial . matColor
	      lightF = sum . map (calcLight eP s i) $ s ^. lights

calcLight :: R3 -> Scene -> Intersection -> Light -> Color
calcLight _ _ _ (AmbientLight a)               = a
calcLight _ _ i (DirectionalLight (Norm3 d) l) = scale (negate (d.*.n)) l
	where Norm3 n = i ^. iNormal
calcLight _ s i (PositionalLight lp lc)        = if inSight s lp (i ^. iPoint)
                                                 then lc else black

inSight :: Scene -> R3 -> R3 -> Bool
inSight scene from to = isNothing hit || hit == Just to
    where hit = (^. iPoint) <$> (findIntersection scene $ rayTo from to)

findIntersection :: Scene -> Ray -> Maybe Intersection
findIntersection s r =
    case intersections of
        []   -> Nothing
        ints -> Just . nearest $ ints
    where intersections    = thoseIntersected (s ^. objects)
          thoseIntersected = catMaybes . map ((flip ($)) r)
          nearest          = head . sortWith (^. iDistance)

