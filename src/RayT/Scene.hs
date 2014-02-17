module RayT.Scene
    ( Object
    , Scene (..)
    , inSight, findIntersection
    , module RayT.Lights
    , module RayT.Rays
    ) where

import GHC.Exts
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes, isNothing)

import RayT.Lights
import RayT.Rays

type Object = Ray -> Maybe Intersection

data Scene  = Scene
    { objects :: [Object]
    , lights  :: [Light]
    }

inSight :: Scene -> R3 -> R3 -> Bool
inSight scene from to = isNothing hit || hit == Just to
    where hit = iPoint <$> (findIntersection scene $ rayTo from to)

findIntersection :: Scene -> Ray -> Maybe Intersection
findIntersection s r =
    case intersections of
        []   -> Nothing
        ints -> Just . nearest $ ints
    where intersections    = thoseIntersected . objects $ s
          thoseIntersected = catMaybes . map ((flip ($)) r)
          nearest          = head . sortWith iDistance

