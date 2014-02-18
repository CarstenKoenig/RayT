-- | provides a datatype `UnitDouble` that are doubles bound between 0 and 1
module RayT.Colors
    ( Color, ColorRGB
    , rgb
    , toColorRGB
    , scale
    , black, white, red, green, blue
    ) where

import Codec.Picture (Pixel8)
import RayT.Utils

type ColorRGB = (Pixel8, Pixel8, Pixel8)
newtype Color = RGB (Double, Double, Double)
    deriving Show

-- * Colors

black :: Color
black = rgb 0 0 0

white :: Color
white = rgb 1 1 1

red :: Color
red = rgb 1 0 0

green :: Color
green = rgb 0 1 0

blue :: Color
blue = rgb 0 0 1

-- * operators

-- | color given by red/green/blue componentes (each between 0 and 1)
rgb :: Double -> Double -> Double -> Color
rgb r g b = RGB (bound r, bound g, bound b)

scale :: Double -> Color -> Color
scale d (RGB (r,g,b))
    | d < 0     = black
    | otherwise = rgb (d*r) (d*g) (d*b)

bound :: Double => Double
bound a
    | a < 0     = 0
    | a > 1     = 1
    | otherwise = a 

toColorRGB :: Color -> ColorRGB
toColorRGB (RGB (r,g,b)) = 
    (scaleUp r, scaleUp g, scaleUp b)
      where scaleUp v =  floor $ 255 * bound v

instance Eq Color where
    RGB (r,g,b) == RGB (r',g',b') = r ~= r' && g ~= g' && b ~= b'

-- | defining some basic operations on Colors
instance Num Color where
    RGB (r,g,b) + RGB (r', g', b') = RGB (r+r', g+g', b+b')
    RGB (r,g,b) - RGB (r', g', b') = RGB (r-r', g-g', b-b')
    RGB (r,g,b) * RGB (r', g', b') = RGB (r*r', g*g', b*b')
    negate (RGB (r,g,b))           = RGB (negate r, negate g, negate b)
    fromInteger a                  = RGB (x, x, x)
                                    where x = fromInteger $ a
    abs _                          = undefined
    signum _                       = undefined