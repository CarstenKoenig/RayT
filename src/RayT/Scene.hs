{-# LANGUAGE TemplateHaskell #-}

module RayT.Scene
    ( Object
    , Scene (..), objects, lights
    , inSight, findIntersection
    , module RayT.Lights
    , module RayT.Rays
    ) where

import GHC.Exts
import Control.Applicative ((<$>))
import Control.Lens hiding (from, to)
import Data.Maybe (catMaybes, isNothing)

import RayT.Lights
import RayT.Rays

type Object = Ray -> Maybe Intersection

data Scene  = Scene
    { _objects :: [Object]
    , _lights  :: [Light]
    }
makeLenses ''Scene


inSight :: Scene -> R3 -> R3 -> Bool
inSight scene from to = isNothing hit || hit == Just to
    where hit = iPoint <$> (findIntersection scene $ rayTo from to)

findIntersection :: Scene -> Ray -> Maybe Intersection
findIntersection s r =
    case intersections of
        []   -> Nothing
        ints -> Just . nearest $ ints
    where intersections    = thoseIntersected $ s ^. objects
          thoseIntersected = catMaybes . map ((flip ($)) r)
          nearest          = head . sortWith iDistance

